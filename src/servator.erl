%%
%% Create a start script from information about a running system
%%
-module(servator).

-export([make/1]).
-export([make_soft_release/1]).
-export([make_release/2]).

-export([system_applications/0]).
-export([user_applications/0]).
-export([get_started_applications/0]).

-export([copy_erlang_erts/1]).
-export([copy_user_applications/1]).
-export([copy_otp_applications/1]).

%% util
-export([make_dir/1]).  %% recursive
-export([copy_dir/2]).  %% recursive
-export([copy_with_mode/2]).
-export([copy_replace/3]).
-export([make_executable/1]).

-compile(export_all).

-include_lib("kernel/include/file.hrl").

-define(Q, $").
-define(NL, "\n").
-define(TAB,  "  ").

-define(ETC, ["etc", "erlang"]).
-define(VAR, ["var", "erlang"]).

%% Nice to set environment variables
%% ERL_CRASH_DUMP=/dev/null
%% Others found in runtime system:
%% ERL_CRASH_DUMP_SECONDS
%% ERL_CRASH_DUMP_NICE
%% ERL_NO_VFORK
%% ERL_NO_KERNEL_POLL
%% ERL_THREAD_POOL_SIZE
%% ERL_MAX_ETS_TABLES_ENV
%% ERL_FULLSWEEP_AFTER
%% ERL_MAX_PORTS
%%
make(AppName0) ->
    make_soft_release(AppName0).

make_soft_release(AppName0) ->
    Rel = "soft",
    make(AppName0,Rel),
    make_release_dir(AppName0, Rel).

make_release(AppName0, Rel0) ->
    AppName = to_string(AppName0),
    Rel = to_string(Rel0),
    make(AppName0,Rel0),
    copy_erlang_erts(AppName),
    copy_user_applications(AppName),
    copy_otp_applications(AppName),
    make_release_dir(AppName0, Rel).

make(AppName0,Rel0) ->
    AppName = to_string(AppName0),
    make_scripts(AppName,to_string(Rel0)),
    make_osx_plist(AppName),
    make_init_d(AppName),
    ok = make_cookie_file(AppName),
    ok.

make_scripts(AppName,Rel0) ->
    Rel = to_string(Rel0),
    Etc = filename:join(?ETC ++ [AppName]),
    Var = filename:join(?VAR ++ [AppName]),
    ok = make_dir(Etc),
    ok = make_dir(Var),
    Start = shell_start_command(AppName,Rel),
    Interactive = shell_interactive_command(AppName,Rel),
    Stop  = shell_stop_command(AppName,Rel),
    Attach = shell_attach_command(AppName,Rel),
    Status = shell_status_command(AppName,Rel),
    Script0 =
	{script,[
		 {r,["case $1 in"]},
		 {r,["start)"]},
		 tab(Start),
		 {r, [?TAB ";;"]},
		 {r,["interactive)"]},
		 tab(Interactive),
		 {r, [?TAB ";;"]},
		 {r, ["stop)"]},
		 tab(Stop),
		 {r, [?TAB ";;"]},
		 {r, ["status)"]},
		 tab(Status),
		 {r, [?TAB ";;"]},
		 {r, ["attach)"]},
		 tab(Attach),
		 {r, [?TAB ";;"]},
		 {r, ["esac"]}]},
    Script1 = tab(Script0),
    Script2 = nl(Script1),
    Script3 = flat({script,
		    [
		     {r,["#!/bin/bash\n"]},
		     {r,["THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR/../../..\" && pwd)`",?NL]},
		     {r,["PREFIX=$THISDIR",?NL]},  %% maybe set to . or whatever
		     {r,["VSN=",Rel,?NL]},
		     {r,["VAR=","$PREFIX","/",Var,?NL]},
		     {r,["ETC=","$PREFIX","/",Etc,?NL]},
		     {r,["export HOME=","$PREFIX","/",Etc,?NL]},
		     if Rel =:= "" ->
			     {r,["ERL=","erl",?NL]};
			true ->
			     {r,["ERL=",
				 filename:join(["$VAR","rel","$VSN",
						"bin","erl"]),?NL]}
		     end
		     | Script2]}),
    Run = filename:join([Etc, to_string(AppName)++".run"]),
    ok = file:write_file(Run, list_to_binary(Script3)),
    io:format("wrote file: ~s\n", [Run]),
    ok = make_executable(Run).

%% Make the serv plist to use with mac os, replace the start/stop
%% sudo launchctl load /etc/erlang/<app>/<app>.plist 
%% sudo launchctl unload /etc/erlang/<app>/<app>.plist 

make_osx_plist(AppName) ->
    Script1 = osx_plist(AppName),
    Script2 = nl(Script1),
    Script3 = flat(Script2),
    FileName = "org.erlang."++to_string(AppName)++".plist",
    PList = filename:join(?ETC++[AppName,FileName]),
    ok = file:write_file(PList, list_to_binary(Script3)),
    io:format("wrote file: ~s\n", [PList]),
    ok.

%% OSX:
%%   cp /etc/erlang/<app>/org.erlang.<app>.plist /System/Library/LaunchDaemons/
%%   sudo launchctl bootstrap system /System/Library/LaunchDaemons/org.erlang.<app>.plist
%%   sudo launchctl kickstart system/org.erlang.<app>.plist
%%   sudo launchctl enable system/org.erlang.<app>.plist
%%
osx_plist(AppName) ->
    Args = init:get_arguments(),
    Name = proplists:get_value(progname, Args, "erl"),
    Executable = os:find_executable(Name),
    ArgsFilePath = filename:join(["/"|?ETC]++
				     [AppName,to_string(start)++".args"]),
    HomeDir = filename:join(["/"|?VAR]++[AppName]),
    User = os:getenv("USER"),

    {script,
     [
      {r, ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"]},
      {r, ["<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\""]},
      {r, ["  \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"]},
      {r, ["<plist version=\"1.0\">"]},
      {r, [?TAB,"<dict>"]},
      {r, [?TAB,?TAB,"<key>Label</key>"]},
      {r, [?TAB,?TAB,"<string>org.erlang.",AppName,"</string>"]},
      {r, [?TAB,?TAB,"<key>EnvironmentVariables</key>"]},
      {r, [?TAB,?TAB,"<dict>"]},
      {r, [?TAB,?TAB,?TAB,"<key>HOME</key>"]},
      {r, [?TAB,?TAB,?TAB,"<string>",HomeDir,"</string>"]},
      {r, [?TAB,?TAB,?TAB,"<key>ERL_CRASH_DUMP</key>"]},
      {r, [?TAB,?TAB,?TAB,"<string>/dev/null</string>"]},
%%    {r, [?TAB,?TAB,?TAB,"<key>DYLD_LIBRARY_PATH</key>"]},
%%    {r, [?TAB,?TAB,?TAB,"<string>/opt/local/lib:</string>"]},
      {r, [?TAB,?TAB,"</dict>"]},
      {r, [?TAB,?TAB,"<key>ProgramArguments</key>"]},
      {r, [?TAB,?TAB,"<array>"]},
      {r, [?TAB,?TAB,"<string>", Executable, "</string>"]},
      {r, [?TAB,?TAB,"<string>-noinput</string>"]},
      {r, [?TAB,?TAB,"<string>-args_file</string>"]},
      {r, [?TAB,?TAB,"<string>",ArgsFilePath,"</string>"]},
      {r, [?TAB,?TAB,"</array>"]},
      {r, [?TAB,?TAB,"<key>UserName</key>"]},
      {r, [?TAB,?TAB,"<string>", User, "</string>"]},
      {r, [?TAB,?TAB,"<key>StandardOutPath</key>"]},
      {r, [?TAB,?TAB,"<string>/dev/null</string>"]},
      {r, [?TAB,?TAB,"<key>StandardErrorPath</key>"]},
      {r, [?TAB,?TAB,"<string>/dev/null</string>"]},
      {r, [?TAB,?TAB,"<key>RunAtLoad</key>"]},
      {r, [?TAB,?TAB,"<true/>"]},
      {r, [?TAB,?TAB,"<key>KeepAlive</key>"]},
      {r, [?TAB,?TAB,"<true/>"]},
      {r, [?TAB,?TAB,"<key>WorkingDirectory</key>"]},
      {r, [?TAB,?TAB,"<string>", HomeDir, "</string>"]},
      {r, [?TAB,"</dict>"]},
      {r, ["</plist>"]}
     ]}.

%%
%% Linux: init.d script
%% Install:
%%        cp etc/init.d/<app>  to /etc/init.d/
%%        chmod +x /etc/init.d/<app>
%%        sudo update-rc.d <app> defaults
%%
make_init_d(AppName) ->
    Initd = filename:join("etc", "init.d"),
    ok = make_dir(Initd),
    Script1 = init_d(AppName),
    Script2 = nl(Script1),
    Script3 = flat(Script2),
    FileName = filename:join([Initd, to_string(AppName)]),
    ok = file:write_file(FileName, list_to_binary(Script3)),
    ok = make_executable(FileName),
    io:format("wrote file: ~s\n", [FileName]),
    ok.

init_d(AppName) ->
    {script,
     [
      {r, ["#! /bin/sh"]},
      {r, ["#"]},
      {r, [""]},
      {r, ["### BEGIN INIT INFO"]},
      {r, ["# Provides:          ", AppName]},
      {r, ["# Required-Start:    $remote_fs $network"]},
      {r, ["# Required-Stop:     $remote_fs $network"]},
      {r, ["# Default-Start:     2 3 4 5"]},
      {r, ["# Default-Stop:      0 1 6"]},
      {r, ["# Short-Description: Starts ", AppName]},
      {r, ["# Description:       Starts a"]},
      {r, ["#                    longer description of ",AppName]},
      {r, ["### END INIT INFO"]},
      {r, [""]},
      {r, ["set -e"]},
      {r, [""]},
      {r, ["PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"]},
      {r, ["DAEMON=/etc/erlang/",AppName,"/",AppName,".run"]},
      {r, ["NAME=\"",AppName,"\""]},
      {r, ["DESC=\"Describe ", AppName, " here\""]},
      {r, [""]},
      {r, ["test -x $DAEMON || exit 0"]},
      {r, [""]},
      {r, ["start()"]},
      {r, ["{"]},
      {r, ["    $DAEMON start"]},
      {r, ["}"]},
      {r, [""]},
      {r, ["stop()"]},
      {r, ["{"]},
      {r, ["    $DAEMON stop"]},
      {r, ["}"]},
      {r, [""]},
      {r, ["case \"$1\" in"]},
      {r, ["    start)"]},
      {r, ["	echo -n \"Starting $DESC: \""]},
      {r, ["	start"]},
      {r, ["	echo \"$NAME.\""]},
      {r, ["    ;;"]},
      {r, ["    stop)"]},
      {r, ["	echo -n \"Stopping $DESC: \""]},
      {r, ["	stop"]},
      {r, ["	echo \"$NAME.\""]},
      {r, ["    ;;"]},
      {r, ["    status)"]},
      {r, ["	$DAEMON status"]},
      {r, ["    ;;"]},
      {r, ["    reload|force-reload)"]},
      {r, ["	echo \"Reloading $DESC configuration files.\""]},
      {r, ["	$DAEMON hup"]},
      {r, ["    ;;"]},
      {r, ["    restart)"]},
      {r, ["	echo -n \"Restarting $DESC: \""]},
      {r, ["	stop"]},
      {r, ["	start"]},
      {r, ["	echo \"$NAME.\""]},
      {r, ["    ;;"]},
      {r, ["    *)"]},
      {r, ["	N=/etc/init.d/$NAME"]},
      {r, ["	echo \"Usage: $N {start|stop|status|restart|reload|force-reload}\" >&2"]},
      {r, ["	exit 1"]},
      {r, ["    ;;"]},
      {r, ["esac"]},
      {r, [""]},
      {r, ["exit 0"]}
     ]}.

make_executable(File) ->
    case file:read_file_info(File) of
	{ok, Info} ->
	    Info2 = Info#file_info { mode = Info#file_info.mode bor 8#111 },
	    file:write_file_info(File, Info2);
	Error ->
	    Error
    end.

su_command() ->
    User = os:getenv("USER"),
    case os:type() of
	{unix,darwin} ->  "su " ++ User ++ " -c ";
	{unix,_} -> "su - " ++ User  ++ " -c "
    end.

%% Argument to set node name
%% -sname x
%% -name x
erl_node_arg() ->
    case string_split(atom_to_list(node()), $@) of
	["nonode", "nohost"] -> [];
	[Name,Host] ->
	    case string_split(Host, $.) of
		[_] -> [{"sname",Name}];
		[_|_] -> [{"name",Name}]
	    end
    end.

client_node_arg() ->
    case string_split(atom_to_list(node()), $@) of
	["nonode", "nohost"] -> " -sname client_$$";
	[_Name,Host] ->
	    case string_split(Host, $.) of
		[_] -> " -sname client_$$";
		[_|_] -> " -name client_$$"
	    end
    end.

%% Write cookie in a cookie file under /etc/erlang/<app>/.erlang.cookie
%% and setup cookie file mode.
make_cookie_file(AppName) ->
    case erlang:get_cookie() of
	nocookie -> 
	    {error,nocookie};
	Cookie ->
	    CookieFile = filename:join(?ETC ++ [AppName,".erlang.cookie"]),
	    file:write_file(CookieFile, Cookie),
	    {ok,Info} = file:read_file_info(CookieFile),
	    Mode = Info#file_info.mode band 8#700,
	    file:change_mode(CookieFile, Mode)
    end.
    
%% -hidden - make sure client node is not attached into a cluster
erl_hidden_arg() ->
    [{"hidden", ""}].

erl_config_arg(AppName) ->
    Args = init:get_arguments(),
    case proplists:get_value(config, Args) of
	undefined -> "";
	SrcConfig0 ->
	    SrcConfig = 
		case filename:extension(SrcConfig0) of
		    [] -> string:concat(SrcConfig0, ".config");
		    ".config" -> 
			SrcConfig0;
		    _Other ->
			io:format("warning: unexpected config file: ~s\n", 
				  [SrcConfig0]),
			SrcConfig0
		end,
	    DstConfig = filename:basename(SrcConfig),
	    DstConfigPath = filename:join(?ETC++[AppName, DstConfig]),
	    %% How is the config file located? 
	    case file:read_file(SrcConfig) of
		{ok,Bin} ->
		    ok = file:write_file(DstConfigPath, Bin),
		    io:format("copied file: ~s to ~s\n",
			      [SrcConfig, DstConfigPath]),
		    DstConfig;
		Error ->
		    io:format("error copy file: ~s : ~p\n", [SrcConfig, Error]),
		    DstConfig
	    end
    end.

erl_smp_arg() ->
    [{"smp", "enabled"}].

%%
%% here we select either:
%%     -s AppName
%% or
%%     -eval "application:ensure_all_started(AppName)"
%%
erl_start_arg(AppName) ->
    [{"s", AppName}].

erl_stop_arg(_AppName) ->
    NodeName = atom_to_list(node()),
    [{"eval", "case rpc:call('"++NodeName++"', init, stop, []) of ok -> erlang:display(ok), erlang:halt(0); {badrpc,R} -> erlang:display(R), erlang:halt(1) end."}].

erl_status_arg(_AppName) ->
    NodeName = atom_to_list(node()),
    [{"eval", "case net_adm:ping('"++NodeName++"') of pong -> erlang:display(up), erlang:halt(0); pang -> erlang:display(down), erlang:halt(1) end."}].

erl_attach_arg(_AppName) ->
    NodeName = atom_to_list(node()),
    [{"remsh", NodeName}].

%%
%% Heart commands
%% FIXME: set environment and setup heart
erl_heart_arg() ->
    [].

make_args(start, AppName, _Rel) ->
    erl_node_arg() ++
	erl_heart_arg() ++
	erl_smp_arg() ++
	erl_start_arg(AppName);
make_args(stop, AppName, _Rel) ->
	erl_hidden_arg() ++
	erl_stop_arg(AppName);
make_args(status, AppName, _Rel) ->
	erl_hidden_arg() ++
	erl_status_arg(AppName);
make_args(attach, AppName, _Rel) ->
	erl_hidden_arg() ++
	erl_attach_arg(AppName).
%%
%% write arg_file: debug this, erlexec is doing something strange
%% with some arguments
%%
emit_args_file(File, Args) ->
    case file:open(File, [write]) of
	{ok,Fd} ->
	    lists:foreach(
	      fun({Opt,""}) ->
		      io:format(Fd, "-~s ", [Opt]);
		 ({Opt,Value}) ->
		      io:format(Fd, "-~s \"~s\" ", [Opt,Value])
	      end, Args),
	    io:format(Fd, "\n", []),
	    file:close(Fd),
	    io:format("wrote file: ~s\n", [File]),
	    ok;
	Error ->
	    io:format("error writing file: ~s: ~p\n", [File, Error]),
	    Error
    end.


%% Generate the start command
shell_start_command(AppName,Rel) ->
    User = os:getenv("USER"),
    Flags0 =
	case erl_config_arg(AppName) of
	    "" -> "";
	    DstConfig ->
		" -config "++filename:join("$ETC", DstConfig)
	end,
    Flags1 = Flags0 ++ " -pa $VAR/rel/$VSN/lib/PATCHES/ebin",
    Start = erl_args(AppName, start, Flags1++" -detached", Rel),
    HomeDir = "$VAR", %% filename:join(["/"|?VAR]++[AppName]),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r,[?TAB, su_command(), ?Q, "(cd ", HomeDir, "; ",
	  "export ERL_CRASH_DUMP=/dev/null; ",
	  Start, " > /dev/null 2>&1", ")", ?Q ]},
      {r,["else"]},
      {r,[?TAB, "(cd ", HomeDir, "; ",
	  "export ERL_CRASH_DUMP=/dev/null; ",
	  Start," > /dev/null 2>&1", ")"]},
      {r,["fi"]}
     ]}.

%% Generate the interactive command
shell_interactive_command(AppName,Rel) ->
    User = os:getenv("USER"),
    Flags0 = 
	case erl_config_arg(AppName) of
	    "" -> "";
	    DstConfig ->
		" -config "++filename:join("$ETC", DstConfig)
	end,
    Flags1 = Flags0 ++ " -pa $VAR/rel/$VSN/lib/PATCHES/ebin",
    HomeDir = "$VAR", %% filename:join(["/"|?VAR]++[AppName]),
    Start = erl_args(AppName, start, Flags0, Rel),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r,[?TAB, su_command(), ?Q, "(cd ", HomeDir, "; ",
	  %% "export ERL_CRASH_DUMP=/dev/null; ",
	  Start, ")", ?Q ]},
      {r,["else"]},
      {r,[?TAB, "(cd ", HomeDir, "; ",
	  %% "export ERL_CRASH_DUMP=/dev/null; ",
	  Start, ")"]},
      {r,["fi"]}
     ]}.

%% Generate the stop command
shell_stop_command(AppName,Rel) ->
    User = os:getenv("USER"),
    NodeArg = client_node_arg(),
    Stop = erl_args(AppName, stop, NodeArg ++ " -noinput", Rel),
    {script,
     [
      {r, ["if [ \"$USER\" != \"", User, "\"", " ]; then"]},
      {r, [?TAB, su_command() ++ "\"" ++  Stop ++ "\""]},
      {r, ["else"]},
      {r, [?TAB, Stop]},
      {r, ["fi"]}
     ]}.

%% Generate the attach command
shell_attach_command(AppName,Rel) ->
    User = os:getenv("USER"),
    NodeArg = client_node_arg(),
    Attach = erl_args(AppName, attach, NodeArg, Rel),
    {script,
     [
      {r, ["if [ \"$USER\" != \"", User, "\"", " ]; then"]},
      {r, [?TAB, su_command() ++ "\"" ++  Attach ++ "\""]},
      {r, ["else"]},
      {r, [?TAB, Attach]},
      {r, ["fi"]}
     ]}.

%% Generate the status command
shell_status_command(AppName,Rel) ->
    User = os:getenv("USER"),
    NodeArg = client_node_arg(),
    Status = erl_args(AppName, status,  NodeArg ++ " -noinput", Rel),
    {script,
     [
      {r, ["if [ \"$USER\" != \"", User, "\"", " ]; then"]},
      {r, [?TAB, su_command() ++ "\"" ++  Status ++ "\""]},
      {r, ["else"]},
      {r, [?TAB, Status]},
      {r, ["fi"]}
     ]}.

erl_args(AppName, Type, Flags, Rel) ->
    %% Args = init:get_arguments(),
    %% Name = proplists:get_value(progname, Args, "erl"),
    %% Executable = os:find_executable(Name),
    %% {ok,_Wd} = file:get_cwd(),
    FileName = to_string(Type)++".args",
    ArgsFileName = filename:join(?ETC++[AppName,FileName]),
    emit_args_file(ArgsFileName, make_args(Type, AppName, Rel)),
    "$ERL " ++ Flags ++ " -args_file " ++ filename:join("$ETC",FileName).

%%
%% Combine script, newline and tabs
%%
tab([H|T]) -> [tab(H)|tab(T)];
tab({script,S}) -> {script,tab(S)};
tab({r,R}) -> {r,[?TAB|R]};
tab([]) -> [].

nl([H|T]) -> [nl(H)|nl(T)];
nl({r,R}) -> {r,[R,?NL]};
nl({script,S}) -> {script,nl(S)};
nl([]) -> [].

flat([H|T]) -> [flat(H)|flat(T)];
flat({r,Row}) -> Row;
flat({script,S}) -> flat(S);
flat([]) -> [].

make_dir(Dir) ->
    Ds = filename:split(Dir),
    make_dir_(Ds, ".").

make_dir_([Dir|Ds], Path) ->
    Path1 = filename:join(Path, Dir),
    io:format("make_dir ~s\n", [Path1]),
    case file:make_dir(Path1) of
	ok ->
	    make_dir_(Ds, Path1);
	{error,eexist} ->
	    make_dir_(Ds, Path1);
	Error ->
	    Error
    end;
make_dir_([], _Path) ->
    ok.

get_ebin_paths(As) ->
    [ filename:join([code:lib_dir(App),"ebin"]) || App <- As ].

get_lib_paths(As) ->
    [ code:lib_dir(App) || App <- As ].

to_string(X) when is_atom(X) -> atom_to_list(X);
to_string(X) when is_list(X) -> X;
to_string(X) when is_binary(X) -> binary_to_list(X).

string_split(String, Chr) ->
    case string:chr(String, Chr) of
	0 -> [String];
	I ->
	    {Before,[Chr|After]} = lists:split(I-1,String),
	    [Before, After]
    end.

get_started_applications() ->
    Apps = [A || {A,_Name,_Vsn} <- application:which_applications()],
    SysApps = system_applications(),
    filter_applications(Apps -- SysApps).

filter_applications(As) ->
    %% FIXME: check if fnotify was started from .erlang!
    As -- [servator,fnotify,error_emacs].

%% List all applications found in the OTP release
system_applications() ->
    LibDir = code:lib_dir(),
    {ok,Apps} = file:list_dir(LibDir),
    system_applications(Apps, LibDir, []).

system_applications([AppVsn|Apps], LibDir, Acc) ->
    App = case string:chr(AppVsn, $-) of
	      0 -> AppVsn;
	      I -> string:sub_string(AppVsn, 1, I-1)
	  end,
    case file:consult(filename:join([LibDir,AppVsn,"ebin",App++".app"])) of
	{ok,[{application,A,_As}|_]} ->
	    system_applications(Apps,LibDir,[A|Acc]);
	{error,_} ->
	    system_applications(Apps,LibDir,Acc)
    end;
system_applications([],_LibDir,Acc) ->
    Acc.
%%
%% Copy erts data
%%   erts-<vsn>/bin
%%       beam | beam.smp, child_setup, epmd, heart, inet_gethost,
%%       erlexec, escript
%%
%%  vsn = erlang:system_info(version)
%%  read root from init:get_arguments()
%%
copy_erlang_erts(AppName) ->
    Beam = "beam.smp",
    Args = init:get_arguments(),
    [Root] = proplists:get_value(root, Args),
    ErtsVsn = "erts-"++erlang:system_info(version),
    SrcDir = filename:join([Root, ErtsVsn, "bin"]),
    DstDir = filename:join(?VAR++[AppName, ErtsVsn, "bin"]),
    make_dir(DstDir),

    lists:foreach(
      fun(File) ->
	      copy_with_mode(filename:join([SrcDir, File]),
			     filename:join([DstDir, File]))
      end, [Beam, "child_setup", "epmd", "heart", "inet_gethost",
	    "erlexec", "escript"]).


user_applications() ->
    SysApps = system_applications(),
    UserApps =
	lists:filter(fun({fnotify,_,_}) -> false;
			({error_emacs,_,_}) -> false;
			({App,_Comment,_Vsn}) ->
			     not lists:member(App, SysApps) end,
		     application:loaded_applications()),
    UApps = lists:map(fun({App,_,_Vsn}) -> App end, UserApps),
    %% list all dependencies
    DepApps0 =
	lists:map(fun(App) ->
			  {ok,Apps} = application:get_key(App, applications),
			  Apps
		  end, UApps),
    %% remove system apps and already listed apps
    DepApps = (lists:usort(lists:flatten(DepApps0)) -- SysApps) -- UApps,
    %% fixme: make this recursive!
    XtraApps = 
	lists:map(
	  fun(App) ->
		  application:load(App),
		  {ok,Descr} = application:get_key(App, description),
		  {ok,Vsn} = application:get_key(App, vsn),
		  {App, Descr, Vsn}
	  end, DepApps),
    UserApps ++ XtraApps.
    
copy_user_applications(AppName) ->
    DstDir = filename:join(?VAR ++ [AppName, "lib"]),
    make_dir(DstDir),
    lists:foreach(
      fun({App,_Descr,Vsn}) ->
	      Src = code:lib_dir(App),
	      Dst = filename:join(DstDir, atom_to_list(App)++"-"++Vsn),
	      copy_dir(Src, Dst)
      end, user_applications()).

otp_applications() ->
    SysApps = system_applications(),
    lists:filter(fun({App,_Comment,_Vsn}) -> lists:member(App, SysApps) end,
		 application:loaded_applications()).

copy_otp_applications(AppName) ->
    DstDir = filename:join(?VAR ++ [AppName, "lib"]),
    make_dir(DstDir),
    lists:foreach(
      fun({App,_Descr,Vsn}) ->
	      Src = code:lib_dir(App),
	      Dst = filename:join(DstDir, atom_to_list(App)++"-"++Vsn),
	      copy_dir(Src, Dst)
      end, otp_applications()).


make_release_dir(AppName, Rel) ->
    RelDir = filename:join(?VAR ++ [AppName, "rel", Rel]),
    RelLibDir = filename:join(RelDir, "lib"),
    make_dir(RelLibDir),

    %% create a patches directory that is ( always search first )
    make_dir(filename:join([RelLibDir, "PATCHES", "ebin"]),
    
    %% symlink otp applications - keep version to make start script happy
    lists:foreach(
      fun({AppI,_Descr,Vsn}) ->
	      App = to_string(AppI),
	      Exist =
		  if Rel =:= "soft" ->
			  code:lib_dir(App);
		     true ->
			  filename:join(["..","..","..","lib",App++"-"++Vsn])
		  end,
	      New = filename:join(RelLibDir,App++"-"++Vsn),
	      ok = symlink(Exist, New)
      end, otp_applications()),

    %% symlink user applications
    lists:foreach(
      fun({AppI,_Descr,Vsn}) ->
	      App = to_string(AppI),
	      Exist =
		  if Rel =:= "soft" ->
			  code:lib_dir(App);
		     true ->
			  filename:join(["..","..","..","lib",App++"-"++Vsn])
		  end,
	      New = filename:join(RelLibDir,App),
	      ok = symlink(Exist, New)
      end, user_applications()),

    %% symlink erts directory
    ErtsVsn = "erts-"++erlang:system_info(version),
    Args = init:get_arguments(),
    [Root] = proplists:get_value(root, Args),
    Exist = if Rel =:= "soft" ->
		    filename:join(Root, ErtsVsn);
	       true ->
		    filename:join(["..", "..", ErtsVsn])
	    end,
    ok = symlink(Exist, filename:join(RelDir, "erts")),

    BinDir = filename:join(RelDir, "bin"),
    make_dir(BinDir),

    SrcDir = filename:join(Root, "bin"),
    %% copy scripts
    lists:foreach(
      fun(File) ->
	      copy_with_mode(filename:join([SrcDir, File]),
			     filename:join([BinDir, File]))
      end, ["erlc", "escript", "start.boot", "start.script" ]),

    %% copy "erl" and patch location
    %% ROOTDIR=filename:join("/", RelDir),
    ROOTDIR="$THISDIR",
    THISDIR="THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR/..\" \\&\\& pwd)`",
    copy_replace(filename:join(SrcDir, "erl"),
		 filename:join(BinDir, "erl"),
		 [{"ROOTDIR=\".*\"", THISDIR++"\n"++"ROOTDIR=\""++ROOTDIR++"\"",[]},
		  {"BINDIR=.*", "BINDIR=$ROOTDIR/erts/bin", []}
		 ]),
    %% make symlinks
    lists:foreach(
      fun(File) ->
	      ok = symlink(
		     filename:join(["..","erts","bin","epmd"]),
		     filename:join([BinDir, File]))
      end, ["epmd"]),
    ok.

%% create symlinks, or check that they are equal if they already exist
symlink(Exist, New) ->
    case file:make_symlink(Exist, New) of
	ok -> ok;
	{error,eexist} ->
	    case file:read_link(New) of
		{ok, Exist} -> ok;
		Error -> Error
	    end;
	Error -> Error
    end.
		    
	    
    

copy_dir(Src, Dst) ->
    make_dir(Dst),
    {ok,Cwd} = file:get_cwd(),
    case file:list_dir(Src) of
	{ok,[]} -> ok;
	{ok,Files0} ->
	    Files = if Src =:= Cwd ->
			    Files0 -- ["var", "etc"];
		       true ->
			    Files0
		    end,
	    lists:foreach(
	      fun([$.|_File]) -> %% ignore all dot files!
		      ok;
		 (File) ->
		      Src1 = filename:join(Src,File),
		      Dst1 = filename:join(Dst,File),
		      case filelib:is_dir(Src1) of
			  true -> copy_dir(Src1, Dst1);
			  false -> copy_with_mode(Src1,Dst1)
		      end
	      end, Files),
	    io:format("copied dir ~s to ~s\n", [Src, Dst]);
	Error ->
	    Error
    end.

copy_with_mode(Src, Dst) ->
    io:format("about to copy ~s\n", [Src]),
    {ok,_Size} = file:copy(Src, Dst),
    io:format("copied file ~s to ~s [~w bytes]\n", [Src, Dst, _Size]),
    {ok,Info} = file:read_file_info(Src),
    file:change_mode(Dst, Info#file_info.mode).

%% Copy Src to Dst line by line and do regular expression replace 
%% while doing it.
copy_replace(Src, Dst, ReplaceList) ->
    {ok,S} = file:open(Src, [read, raw, {read_ahead, 1024}]),
    {ok,D} = file:open(Dst, [write, raw]),
    try copy_replace_(S, D, ReplaceList) of
	ok ->
	    ok;
	Error ->
	    Error
    catch
	error:Reason ->
	    {error, Reason}
    after
	file:close(S),
	file:close(D)
    end,
    {ok,Info} = file:read_file_info(Src),
    file:change_mode(Dst, Info#file_info.mode).
    

copy_replace_(S, D, ReplaceList) ->
    case file:read_line(S) of
	{ok,Data} ->
	    Data1 = replace_data(Data, ReplaceList),
	    file:write(D, Data1),
	    copy_replace_(S, D, ReplaceList);
	eof ->
	    ok;
	Error ->
	    Error
    end.

replace_data(Data, [{RE,Replacement,Options}|REs]) ->
    case re:replace(Data, RE, Replacement, Options) of
	Data -> replace_data(Data, REs);
	Data1 -> replace_data(iolist_to_binary(Data1), REs)
    end;
replace_data(Data, []) ->
    Data.
