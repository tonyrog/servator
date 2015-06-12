%%
%% Create a start script from information about a running system
%%
-module(servator).

-compile(export_all).

-include_lib("kernel/include/file.hrl").


-define(Q, $").
-define(NL, "\n").
-define(TAB,  "  ").

-define(ETC, ["etc", "erlang"]).
-define(VAR, ["var", "erlang"]).

make(AppName0) ->
    AppName = to_string(AppName0),
    make_scripts(AppName),
    make_osx_plist(AppName),
    ok.
    

make_scripts(AppName) ->
    Etc = filename:join(?ETC ++ [AppName]),
    Var = filename:join(?VAR ++ [AppName]),
    ok = make_dir(Etc),
    ok = make_dir(Var),
    Start = shell_start_command(AppName),
    Stop  = shell_stop_command(AppName),
    Attach = shell_attach_command(AppName),
    Status = shell_status_command(AppName),
    Script0 =
	{script,[
		 {r,["case $1 in"]},
		 {r,["start)"]},
		 tab(Start),
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
    Script3 = flat({script,[{r,["#!/bin/bash\n"]} | Script2]}),
    Run = filename:join([Etc, to_string(AppName)++".run"]),
    ok = file:write_file(Run, list_to_binary(Script3)),
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
      {r, [?TAB,"</dict>"]},
      {r, ["</plist>"]}
     ]}.


%% Install 
install_script() ->
    ok.

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

erl_node_arg(OtherName) ->
    case string_split(atom_to_list(node()), $@) of
	["nonode", "nohost"] -> [];
	[_Name,Host] ->
	    case string_split(Host, $.) of
		[_] -> [{"sname",OtherName}];
		[_|_] -> [{"name",OtherName}]
	    end
    end.

erl_client_name() ->
    erl_node_arg("client-"++os:getpid()).

%% -setcookie cookie
erl_cookie_arg() ->
    case erlang:get_cookie() of
	nocookie -> [];
	Cookie -> [{"setcookie", atom_to_list(Cookie)}]
    end.

%% -hidden - make sure client node is not attached into a cluster
erl_hidden_arg() ->
    [{"hidden", ""}].


erl_config_arg(AppName) ->
    Args = init:get_arguments(),
    case proplists:get_value(config, Args) of
	undefined -> [];
	Config ->
	    ConfigPath = filename:join(?ETC++[AppName, Config]),
	    %% How is the config file located? 
	    case file:read_file(Config) of
		{ok,Bin} ->
		    ok = file:write_file(ConfigPath, Bin),
		    io:format("copied file: ~s to ~s\n", [Config, ConfigPath]),
		    [{"config", filename:join("/", ConfigPath)}];
		Error ->
		    io:format("error copy file: ~s : ~p\n", [Config, Error]),
		    [{"config", filename:join("/", ConfigPath)}]
	    end
    end.

%% Generate all paths needed
%% -pa <path>
erl_path_arg() ->
    Applications = get_started_applications(),
    Paths = get_paths(Applications),
    [ {"pa",P} || P <- Paths].

erl_foreground_arg() ->
    [{"noinput", ""}].

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
%% Environment variables
%%
erl_env_arg() ->
    [{"env", "ERL_CRASH_DUMP /dev/null"}].

%%
%% Heart commands
%% FIXME: set environment and setup heart
erl_heart_arg() ->
    [].

make_args(start, AppName) ->
    erl_node_arg() ++
	erl_cookie_arg() ++
	erl_config_arg(AppName) ++
	erl_path_arg() ++
	erl_env_arg() ++
	erl_heart_arg() ++
	erl_smp_arg() ++
	erl_start_arg(AppName);
make_args(stop, AppName) ->
    erl_client_name() ++
	erl_cookie_arg() ++
	erl_foreground_arg() ++
	erl_env_arg() ++
	erl_hidden_arg() ++
	erl_stop_arg(AppName);
make_args(status, AppName) ->
    erl_client_name() ++
	erl_cookie_arg() ++
	erl_foreground_arg() ++
	erl_env_arg() ++
	erl_hidden_arg() ++
	erl_status_arg(AppName);
make_args(attach, AppName) ->
    erl_client_name() ++
	erl_cookie_arg() ++
	erl_env_arg() ++
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
		      io:format(Fd, "-~s\n", [Opt]);
		 ({Opt,Value}) ->
		      io:format(Fd, "-~s \"~s\"\n", [Opt,Value])
	      end, Args),
	    file:close(Fd),
	    io:format("wrote file: ~s\n", [File]),
	    ok;
	Error ->
	    io:format("error writing file: ~s: ~p\n", [File, Error]),
	    Error
    end.


%% Generate the start command
shell_start_command(AppName) ->
    User = os:getenv("USER"),
    Start = erl_args(AppName, start, " -detached"),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r,[?TAB, su_command(), ?Q, "(cd ", filename:join(["/"|?VAR]++[AppName]), "; ",
	  Start, " > /dev/null 2>&1", ")", ?Q ]},
      {r,["else"]},
      {r,[?TAB, "(cd ", filename:join(["/"|?VAR]++[AppName]), "; ", Start, ")"]},
      {r,["fi"]}
     ]}.

%% Generate the stop command
shell_stop_command(AppName) ->
    User = os:getenv("USER"),
    Stop = erl_args(AppName, stop, " -noinput"),
    {script,
     [
      {r, ["if [ \"$USER\" != \"", User, "\"", " ]; then"]},
      {r, [?TAB, su_command() ++ "\"" ++  Stop ++ "\""]},
      {r, ["else"]},
      {r, [?TAB, Stop]},
      {r, ["fi"]}
     ]}.

%% Generate the attach command
shell_attach_command(AppName) ->
    User = os:getenv("USER"),
    Attach = erl_args(AppName, attach,  " -noinput"),
    {script,
     [
      {r, ["if [ \"$USER\" != \"", User, "\"", " ]; then"]},
      {r, [?TAB, su_command() ++ "\"" ++  Attach ++ "\""]},
      {r, ["else"]},
      {r, [?TAB, Attach]},
      {r, ["fi"]}
     ]}.

%% Generate the status command
shell_status_command(AppName) ->
    User = os:getenv("USER"),
    Status = erl_args(AppName, status,  " -noinput"),
    {script,
     [
      {r, ["if [ \"$USER\" != \"", User, "\"", " ]; then"]},
      {r, [?TAB, su_command() ++ "\"" ++  Status ++ "\""]},
      {r, ["else"]},
      {r, [?TAB, Status]},
      {r, ["fi"]}
     ]}.

erl_args(AppName, Type, Flags) ->
    Args = init:get_arguments(),
    Name = proplists:get_value(progname, Args, "erl"),
    Executable = os:find_executable(Name),
    {ok,_Wd} = file:get_cwd(),
    ArgsFilePath = ?ETC++[AppName,to_string(Type)++".args"],
    ArgsFileName = filename:join(ArgsFilePath),
    emit_args_file(ArgsFileName, make_args(Type, AppName)),
    Executable ++ Flags ++ " -args_file " ++ 
	filename:join(["/" | ArgsFilePath]).

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

get_paths([App|As]) ->
    [ filename:join([code:lib_dir(App),"ebin"]) | get_paths(As)];
get_paths([]) ->
    [].

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
