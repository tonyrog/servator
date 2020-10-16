%%
%% Create a start script from information about a running system
%%
-module(servator).

-export([make/1]).
-export([make_soft_release/1]).
-export([make_appimage/1]).
-export([make_starexec/1]).
-export([make_osxapp/1]).
-export([make_win32app/1]).
-export([make_release/1, make_release/3]).


-export([system_applications/0]).
-export([otp_applications/0]).
-export([user_applications/0, user_applications/1]).
-export([depend_applications/1, depend_user_applications/1]).
-export([get_started_applications/0]).
-export([get_started_args/0]).

-export([copy_erlang_erts/2]).
-export([copy_user_applications/2]).
-export([copy_otp_applications/2]).

%% util
-export([make_dir/1]).  %% recursive
-export([copy_dir/2]).  %% recursive
-export([copy_app/2]).  %% recursive
-export([copy_with_mode/2]).
-export([copy_replace/3]).
-export([make_executable/1]).

%% debug
-export([installation_etc_dir/2]).
-export([installation_var_dir/2]).
-export([get_ebin_paths/1]).
-export([get_lib_paths/1]).
-export([make_args/3]).
-export([get_config_filenames/0]).

%% -compile(export_all).

-include_lib("kernel/include/file.hrl").



-define(Q, $").
-define(BSLASH, $\\).
-define(NL, "\n").
-define(TAB,  "  ").

-define(ETC, ["etc", "erlang"]).
-define(VAR, ["var", "erlang"]).

-define(TARGET_ETC, ["/", "etc", "erlang"]).
-define(TARGET_VAR, ["/", "var", "erlang"]).

%%Defaults
-define(HEART_BEAT_TIMEOUT, 20). %% ?? FIXME ??

%% -define(dbg(F,A), ok).
-define(dbg(F,A), io:format((F), (A))).
%% Nice to set environment variables
%% ERL_CRASH_DUMP=/dev/null ?? FIXME Does not work ??
%% Others found in runtime system:
%% ERL_CRASH_DUMP_SECONDS=0 No crash dump !!
%% ERL_CRASH_DUMP_NICE
%% ERL_NO_VFORK
%% ERL_NO_KERNEL_POLL
%% ERL_THREAD_POOL_SIZE
%% ERL_MAX_ETS_TABLES_ENV
%% ERL_FULLSWEEP_AFTER
%% ERL_MAX_PORTS
%%
set_build_type(Type) ->
    put(build_type, Type).

get_build_type() ->
    get(build_type).


make([AppName]) when is_atom(AppName) -> %% from shell
    make(AppName);
make(AppName0) ->
    make_soft_release(AppName0).

make_soft_release(AppName0) ->
    Rel = "soft",
    make(AppName0,Rel,release),
    AppName = to_string(AppName0),
    make_release_dir(AppName, Rel).

make_release([AppName]) when is_atom(AppName) -> %% from shell 
    make_release(AppName);
make_release(AppName) ->
    io:format("Make a release of application ~p\n", [AppName]),
    application:load(AppName),
    {ok,Rel} = application:get_key(AppName, vsn),
    make_release(AppName, Rel, release).

make_appimage([AppName]) when is_atom(AppName) -> %% from shell 
    make_appimage(AppName);
make_appimage(AppName) ->
    io:format("Make a appimage of application ~p\n", [AppName]),
    application:load(AppName),
    {ok,Rel} = application:get_key(AppName, vsn),
    make_release(AppName, Rel, appimage).

make_starexec([AppName]) when is_atom(AppName) -> %% from shell 
    make_starexec(AppName);
make_starexec(AppName) ->
    io:format("Make a appimage of application ~p\n", [AppName]),
    application:load(AppName),
    {ok,Rel} = application:get_key(AppName, vsn),
    make_release(AppName, Rel, starexec).

make_osxapp([AppName]) when is_atom(AppName) -> %% from shell 
    make_osxapp(AppName);
make_osxapp(AppName) ->
    io:format("Make a mac os x app ~p\n", [AppName]),
    application:load(AppName),
    {ok,Rel} = application:get_key(AppName, vsn),
    make_release(AppName, Rel, osxapp).

make_win32app([AppName]) when is_atom(AppName) -> %% from shell 
    make_win32app(AppName);
make_win32app(AppName) ->
    io:format("Make a win32 app ~p\n", [AppName]),
    application:load(AppName),
    {ok,Rel} = application:get_key(AppName, vsn),
    make_release(AppName, Rel, win32app).

make_release(AppName0, Rel0, BuildType) ->
    set_build_type(BuildType),
    AppName = to_string(AppName0),
    Rel = to_string(Rel0),
    make(AppName0,Rel0,BuildType),
    copy_erlang_erts(AppName,Rel),
    copy_user_applications(AppName,Rel),
    copy_otp_applications(AppName,Rel),
    make_release_dir(AppName, Rel).

make(AppName0,Rel0,BuildType) ->
    AppName = to_string(AppName0),
    Rel     = to_string(Rel0),
    Var     = installation_var_dir(AppName,Rel,[AppName]),
    EtcRel  = installation_etc_dir(AppName,Rel,[AppName,Rel]),
    ok = make_dir(EtcRel),
    ok = make_dir(Var),
    ok = copy_configs(AppName,Rel),
    if BuildType =/= appimage,
       BuildType =/= osxapp,
       BuildType =/= win32app ->
	    make_scripts(AppName,Rel),
	    make_apprun(AppName,Rel,BuildType);
       true ->
	    ok
    end,
    if BuildType =:= starexec ->
	    make_starexec_run(AppName,Rel);
       BuildType =:= osxapp ->
	    make_osx_plist(AppName,Rel),
	    make_osx_info_plist(AppName,Rel),
	    make_osxapp_exec(AppName,Rel);
       BuildType =:= win32app ->
	    make_win32app_exec(AppName,Rel);
       BuildType =:= appimage ->
	    copy_appimage_desktop(AppName,Rel),
	    copy_appimage_metadata(AppName,Rel),
	    copy_appimage_icon(AppName,Rel),
	    make_appimage_AppRun(AppName,Rel);
       true ->
	    make_osx_plist(AppName,Rel),
	    make_init_d(AppName,Rel),
	    ok = make_installation_script(AppName,Rel)
    end,
    case make_cookie_file(AppName,Rel) of
	ok -> ok;
	{error,nocookie} -> ok
    end,
    if BuildType =/= appimage,
       BuildType =/= osxapp,
       BuildType =/= win32app ->
	    ok = make_release_file(AppName,Rel);
       true ->
	    ok
    end,
    ok.

%% return the installation directory name
%% under which every thing is created
installation_root_dir(AppName,Rel) ->
    case get_build_type() of
	appimage ->
	    AppName ++ ".AppDir";
	osxapp ->
	    string:titlecase(AppName) ++ ".app";
	win32app ->
	    string:titlecase(AppName) ++ "-" ++ Rel;
	_ ->
	    AppName ++ "-" ++ Rel
    end.

installation_etc_dir(AppName,Rel) ->
    installation_etc_dir(AppName,Rel,[]).
installation_etc_dir(AppName,Rel,Path) ->
    RootDir = installation_root_dir(AppName,Rel),
    case get_build_type() of
	starexec ->
	    filename:join([RootDir,"bin"|?ETC]++Path);
	appimage ->
	    RootDir;
	osxapp ->
	    filename:join([RootDir,"Contents","Resources"]);
	win32app ->
	    RootDir;
	_ ->
	    filename:join([RootDir|?ETC]++Path)
    end.

installation_var_dir(AppName,Rel) ->
    installation_var_dir(AppName,Rel,[]).
installation_var_dir(AppName,Rel,Path) ->
    RootDir = installation_root_dir(AppName,Rel),
    case get_build_type() of
	starexec ->
	    filename:join([RootDir,"bin"|?VAR]++Path);
	appimage ->
	    case Path of
		[AppName] -> filename:join([RootDir,"bin"]);
		[AppName,"rel",_Rel] -> filename:join([RootDir,"bin"]);
		[AppName,_ErtsVsn,"bin"] -> filename:join([RootDir,"bin"]);
		[AppName,"lib"] -> filename:join([RootDir,"lib"])
	    end;
	win32app ->
	    case Path of
		[AppName] -> filename:join([RootDir,"bin"]);
		[AppName,"rel",_Rel] -> filename:join([RootDir,"bin"]);
		[AppName,_ErtsVsn,"bin"] -> filename:join([RootDir,"bin"]);
		[AppName,"lib"] -> filename:join([RootDir,"lib"])
	    end;
	osxapp ->
	    MacOSPath = filename:join([RootDir,"Contents","MacOS"]),
	    case Path of
		[AppName] -> filename:join([MacOSPath,"bin"]);
		[AppName,"rel",_Rel] -> filename:join([MacOSPath,"bin"]);
		[AppName,_ErtsVsn,"bin"] -> filename:join([MacOSPath,"bin"]);
		[AppName,"lib"] -> filename:join([MacOSPath,"lib"])
	    end;
	_ ->
	    filename:join([RootDir|?VAR]++Path)
    end.

make_scripts(AppName,Rel) ->
    RootVar = filename:join(?VAR ++ [AppName]),
    RootEtc = filename:join(?ETC ++ [AppName]),
    Etc = installation_etc_dir(AppName,Rel,[AppName]),
    %% Var = installation_var_dir(AppName,Rel,[AppName]),
    Home = "export HOME=$PREFIX/"++RootEtc,
    Start = shell_start_command(AppName,Rel,Home),
    Interactive = shell_interactive_command(AppName,Rel,Home),
    Stop  = shell_stop_command(AppName,Rel,Home),
    Attach = shell_attach_command(AppName,Rel,Home),
    Status = shell_status_command(AppName,Rel,Home),
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
		     {r,["#!/bin/sh\n"]},
		     {r,["THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR/../../..\" && pwd)`",?NL]},
		     {r,["PREFIX=$THISDIR",?NL]},
		     {r,["if [ \"$PREFIX\" = \"/\" ]; then",?NL]},
		     {r,["    PREFIX=\"\"",?NL]},
		     {r,["fi",?NL]},
		     {r,["VSN=",Rel,?NL]},
		     {r,["VAR=","$PREFIX","/",RootVar,?NL]},
		     {r,["ETC=","$PREFIX","/",RootEtc,?NL]},
		     %% fixme? maybe copy all --opt [arg] data?
		     {r,["OPTS=\"\"",?NL]},
		     {r,[Home,?NL]},
		     if Rel =:= "" ->
			     {r,["ERL=","erl",?NL]};
			true ->
			     {r,["ERL=",
				 filename:join(["$VAR","rel","$VSN",
						"bin","erl"]),?NL]}
		     end
		     | Script2]}),

    Run = filename:join([Etc,Rel,to_string(AppName)++".run"]),
    ok = file:write_file(Run, list_to_binary(Script3)),
    ?dbg("wrote file: ~s\n", [Run]),
    ok = make_executable(Run).

%% AppImage structure:
%% AppRun           -- executable
%% <app>.desktop    -- desktop file
%% <app>.png        -- hmm???
%% usr/share/icons/hicolor/<app>.png  -- hmm2??
%% usr/share/metainfo/<app>.appdata.xml
%% .DirIcon -> usr/share/icons/hicolor/<app>.png  (symlink)
%% bin/              -- erlang vm binaries
%% lib/              -- erlang libraries and applications
%%
%% Make the AppRun start script, calling the <app>.apprun
make_appimage_AppRun(AppName,Rel) ->
    Detached = erl_detached_arg(),
    NoShell  = erl_noshell_arg(),
    NoInput  = erl_noinput_arg(),
    StartArgs = NoShell++NoInput++Detached++make_args(start, AppName, Rel),
    Pipe = if Detached =/= [] ->
		   erl_ignore_stdout();
	      true ->
		   ""
	   end,
    io:format("StartArgs = ~p\n", [StartArgs]),
    Script =
	flat({script,
	      [{r, ["#!/bin/sh\n"]},
	       {r,["THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR\" && pwd)`",?NL]},
	       {r,["PREFIX=$THISDIR",?NL]},
	       {r,["if [ \"$PREFIX\" = \"/\" ]; then",?NL]},
	       {r,["    PREFIX=\"\"",?NL]},
	       {r,["fi",?NL]},
	       %% {r,["export HOME=$PREFIX",?NL]},
	       {r,["unset ERL_LIBS",?NL]},
	       {r, ["$PREFIX/bin/erl ", erl_config_flags(AppName), " ",
		    format_args(StartArgs),
		    " -extra \"$@\"", Pipe, ?NL]}
	      ]}),
    AppRunFile = filename:join(installation_root_dir(AppName,Rel),"AppRun"),
    ok = file:write_file(AppRunFile, list_to_binary(Script)),
    ok = make_executable(AppRunFile).

%% copy priv/<app>.desktop => AppDir/<app>.desktop
copy_appimage_desktop(AppName, Rel) ->
    Desktop = AppName ++ ".desktop",
    AppDesktop0 = filename:join(code:priv_dir(AppName), Desktop),
    {AppDesktop,Replace}  =
	case filelib:is_regular(AppDesktop0) of
	    false ->
		{filename:join(code:priv_dir(servator), "app.desktop"),
		 [{"\\${APP}", AppName, [global]},
		  {"\\${APPNAME}", string:titlecase(AppName),[global]}]};
	    true ->
		{AppDesktop0, []}
	end,
    DesktopDest1 = filename:join(installation_root_dir(AppName,Rel),Desktop),
    copy_replace(AppDesktop, DesktopDest1, Replace),
    ok.

%% copy priv/<app>.appdata.xml => AppDir/usr/share/metainfo/<app>.appdata.xml
%% if appdata does not exist a template is copied from servator
copy_appimage_metadata(AppName, Rel) ->
    DataFile = AppName ++ ".appdata.xml",
    AppData0 = filename:join(code:priv_dir(AppName), DataFile),
    {AppData,Replace}  =
	case filelib:is_regular(AppData0) of
	    false ->
		{filename:join(code:priv_dir(servator), "app.appdata.xml"),
		 [{"\\${APP}", AppName, [global]},
		  {"\\${APPNAME}", string:titlecase(AppName),[global]}]};
	    true ->
		{AppData0, []}
	end,
    Share = ["usr", "share", "metainfo"],
    MetaDir = filename:join([installation_root_dir(AppName,Rel)|Share]),
    make_dir(MetaDir),
    AppDataDest = filename:join(MetaDir,DataFile),
    copy_replace(AppData, AppDataDest, Replace),
    ok.

%% copy priv/<app>.png => AppDir/<app>.png
%% copy priv/<app>.png => AppDir/usr/share/icons/hicolor/<app>.png
%% symlink AppDir/.DirIcon -> usr/share/icons/hicolor/<app>.png
copy_appimage_icon(AppName, Rel) ->
    IconFile = AppName ++ ".png",
    AppIcon0 = filename:join(code:priv_dir(AppName), IconFile),
    AppIcon  =
	case filelib:is_regular(AppIcon0) of
	    false ->
		filename:join(code:priv_dir(servator), "app.png");
	    true ->
		AppIcon0
	end,

    IconDest1 = filename:join(installation_root_dir(AppName,Rel),IconFile),
    copy_with_mode(AppIcon, IconDest1),

    Share = ["usr", "share", "icons", "hicolor"],
    IconDir2  = filename:join([installation_root_dir(AppName,Rel)|Share]),
    IconDest2 = filename:join(IconDir2,IconFile),

    make_dir(IconDir2),
    copy_with_mode(AppIcon, IconDest2),
    IconDir3  = filename:join(Share),
    IconDest3 = filename:join(IconDir3,IconFile),
    symlink(IconDest3,
	    filename:join(installation_root_dir(AppName,Rel),".DirIcon")),
    ok.

%% apprun script similar to regular run script but no user switch
make_apprun(AppName,Rel,BuildType) ->
    RootVar = filename:join(?VAR ++ [AppName]),
    RootEtc = filename:join(?ETC ++ [AppName]),
    Etc = installation_etc_dir(AppName,Rel,[AppName]),
    %% Var = installation_var_dir(AppName,Rel,[AppName]),
    %% ok = make_dir(filename:join(Etc,Rel)),
    Home = "export HOME=$PREFIX/"++RootEtc,
    Script0 = shell_start_apprun(AppName,Rel,Home,true,BuildType),
    Script1 = tab(Script0),
    Script2 = nl(Script1),
    Script3 = flat({script,
		    [
		     {r,["#!/bin/sh\n"]},
		     {r,["THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR/../../..\" && pwd)`",?NL]},
		     {r,["PREFIX=$THISDIR",?NL]},  %% maybe set to . or whatever
		     {r,["if [ \"$PREFIX\" = \"/\" ]; then",?NL]},
		     {r,["    PREFIX=\"\"",?NL]},
		     {r,["fi",?NL]},
		     {r,["VSN=",Rel,?NL]},
		     {r,["VAR=","$PREFIX","/",RootVar,?NL]},
		     {r,["ETC=","$PREFIX","/",RootEtc,?NL]},
		     {r,["OPTS=\"\"",?NL]},
		     {r,[Home,?NL]},
		     if Rel =:= "" ->
			     {r,["ERL=","erl",?NL]};
			true ->
			     {r,["ERL=",
				 filename:join(["$VAR","rel","$VSN",
						"bin","erl"]),?NL]}
		     end
		     | Script2]}),

    AppRun = filename:join([Etc,to_string(AppName)++".apprun"]),
    ok = file:write_file(AppRun, list_to_binary(Script3)),
    ?dbg("wrote file: ~s\n", [AppRun]),
    ok = make_executable(AppRun).    


%% Make a starexec run script in bin/starexec_run_default
%% Starexec scripts are called with two arguments
%% $1 absolute path to input faile
%% $2 absolute to output directory
%% 
make_starexec_run(AppName,Rel) ->
    AppRunName = AppName++".apprun",
    AppRun = filename:join(["$PREFIX"]++?ETC++[AppName,AppRunName]),
    ok = make_dir(filename:join(installation_root_dir(AppName,Rel),"bin")),
    lists:foreach(
      fun(Config) ->
	      Script =
		  flat({script,
			[{r, ["#!/bin/sh\n"]},
			 {r,["THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR\" && pwd)`",?NL]},
			 {r,["PREFIX=$THISDIR",?NL]},
			 {r,["if [ \"$PREFIX\" = \"/\" ]; then",?NL]},
			 {r,["    PREFIX=\"\"",?NL]},
			 {r,["fi",?NL]},
			 {r, ["export CONFIGFILE=", Config, ?NL]},
			 {r, [AppRun," --outdir=\"$1\" \"$2\"",?NL]}
			]}),
	      Run = filename:basename(Config, ".config"),
	      RunFile = "starexec_run_"++Run,
	      StarExecFile = filename:join(
			       [installation_root_dir(AppName,Rel),"bin",
				RunFile]),
	      ok = file:write_file(StarExecFile, list_to_binary(Script)),
	      ok = make_executable(StarExecFile)
      end, get_config_filenames()).
    
%%
%% Create a release file just containing the release version
%% Installed under etc/erlang/<app>/rel/release
%%
make_release_file(AppName,Rel) ->
    ReleaseFile = installation_etc_dir(AppName,Rel,
				       [AppName,Rel,"release"]),
    file:write_file(ReleaseFile, list_to_binary(Rel)).

%% Mac OS X app structure:
%% <App>.app
%%   Contents
%%     Info.plist       -- 
%%     PkgInfo          -- needed for what?
%%     Resources        -- 
%%       AppIcon.icns
%%      <app>.config    -- right place?
%%     MacOS
%%       <App>          -- executable
%%       bin
%%       lib
%%

make_osxapp_exec(AppName,Rel) ->
    Pipe = erl_ignore_stdout(),
    StartArgs = erl_noshell_arg()++make_args(start, AppName, Rel),
    AppName1 = string:titlecase(AppName),
    Script =
	flat({script,
	      [{r, ["#!/bin/sh\n"]},
	       {r,["THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR\" && pwd)`",?NL]},
	       {r,["PREFIX=$THISDIR",?NL]},
	       {r,["if [ \"$PREFIX\" = \"/\" ]; then",?NL]},
	       {r,["    PREFIX=\"\"",?NL]},
	       {r,["fi",?NL]},
	       %% {r,["export HOME=$PREFIX",?NL]},
	       {r,["unset ERL_LIBS",?NL]},
	       {r,["export WX_APP_TITLE=",?Q,AppName1,?Q,?NL]},
	       {r,["export WX_APP_ICON=",
		   ?Q,"$PREFIX/../Resources/",AppName++".png",?Q,?NL]},
	       {r, ["$PREFIX/bin/erl ", erl_config_flags(AppName), " ",
		    format_args(StartArgs),
		    " -extra \"$@\"", Pipe, ?NL]}
	      ]}),
    AppRunFile = filename:join([installation_root_dir(AppName,Rel),
				"Contents","MacOS",AppName1]),
    ok = file:write_file(AppRunFile, list_to_binary(Script)),
    ok = make_executable(AppRunFile).

make_win32app_exec(_AppName,_Rel) ->
    %% FIXME:
    %% Copy and modifiy the win32 template application
    %% for starting erlang applications
    ok.
%%
%% Make an installation script
%% Assume cd into the release structure where the INSTALL script is
%% executed.
%% The script looks something like
%% 
%% cp
%%   -R [--recursive]      recursive                       [mac,linux,busybox]
%%   -P [--no-dereference] never follow symbolic links     [mac,linux,busybox]
%%   -n [--no-clobber]     do not overwrite existing files [mac,linux]
%%   -p [--preserve=..]    preserve mode,ownership,timestamp [mac,linux,busybox]
%%
%% #!/bin/sh
%%    cp -RPpn var/erlang/<app>/*  /var/erlang/<app>/
%%    cp -RPpn etc/erlang/<app>/*  /etc/erlang/<app>/
%%    if [ ! -f /etc/erlang/<app>/<f1>.config ]; then
%%       cp /etc/erlang/<app>/<rel>/<f1>.config /etc/erlang/<app>/<f1>.config
%%    else
%%       echo "servator: do not forget to update f1.config"
%%    fi
%%    if [ ! -f /etc/erlang/<app>/<f2>.config ]; then
%%       cp /etc/erlang/<app>/<rel>/<f2>.config /etc/erlang/<app>/<f2>.config
%%    else
%%       echo "servator: do not forget to update f2.config"
%%    fi
%%    ...
%%    cp -p /etc/erlang/<app>/<rel>/<app>.run /etc/erlang/<app>/<app>.run
%%    if [ -f /etc/erlang/<app>/release ]; then
%%       OLD=`cat /etc/erlang/<app>/release`
%%    fi
%%    cp -p /etc/erlang/<app>/<rel>/release /etc/erlang/<app>/release
%%

make_installation_script(AppName, Rel) when 
      is_list(AppName), is_list(Rel) ->
    %% TARGET DIRECTORIES
    %% /var/erlang/<app>
    TargetVar = filename:join(?TARGET_VAR++[AppName]),
    %% /var/erlang/<app>/rel/<rel>
    TargetRelDir = filename:join([TargetVar,"rel",Rel]),
    %% /var/erlang/<app>/rel/<rel>/lib
    TargetRelLibDir = filename:join(TargetRelDir, "lib"),
    %% /var/erlang/<app>/rel/<rel>/lib/PATCHES/ebin
    _PatchedDir = filename:join([TargetRelLibDir, "PATCHES", "ebin"]),
    %% /etc/erlang/<app>
    TargetEtc = filename:join(?TARGET_ETC++[AppName]),
    %% /etc/erlang/<app>/<rel>
    TargetEtcRel = filename:join(TargetEtc, Rel),
    
    %% SOURCE DIRECTORIES
    %% var/erlang/<app>
    Var = filename:join(?VAR ++ [AppName]),
    %% var/erlang/<app>
    Etc = filename:join(?ETC ++ [AppName]),
    %% var/erlang/<app>/rel/<rel>
    _RelDir = filename:join([Var, "rel", Rel]),
    %% etc/erlang/<app>/<rel>
    _EtcRel = filename:join(?ETC++[Rel]),

    Configs = 
	lists:append(
	  [ [{r,["if [ ! -f ", filename:join(TargetEtc, File),
		 " ]; then"]},
	     {r,[?TAB,"$NSUDO \"cp -p ",
		 filename:join(TargetEtcRel, File)," ",
		 filename:join(TargetEtc, File), "\""]},
	     {r,["else"]},
	     {r,[?TAB,"echo 'servator: do not forget to update ",
		 filename:join(TargetEtc, File),
		 "'"]},
	     {r,["fi"]}] || File <- get_config_filenames()]),

    Script0 =
	[
	 {r, ["#!/bin/sh"]},
	 {r, ["# install and upgrade script"]},
	 {r, ["# Get name of the user"]},
	 {r, ["if [ \"$1\" = \"\" ]; then"]},
	 {r, [?TAB,"XUSER=`logname`"]},
	 {r, ["else"]},
	 {r, [?TAB,"XUSER=$1"]},
	 {r, ["fi"]},
	 {r, ["# Set sudo and no sudo script"]},
	 {r, ["XUID=`id -u`"]},
	 {r, ["if [ $XUID -eq 0 ]; then"]},
	 {r, [?TAB,"SUDO=\"sh -c \""]},
	 {r, [?TAB,"NSUDO=\"sudo -u $XUSER sh -c \""]},
	 {r, ["else"]},
	 {r, [?TAB,"SUDO=\"sudo sh -c \""]},
	 {r, [?TAB,"NSUDO=\"sh -c \""]},
	 {r, ["fi"]},
	 {r, ["if [ ! -d ", TargetVar, " -o ! -d ",TargetEtc," ]; then"]},
	 {r, [?TAB,"$SUDO \"mkdir -p ", TargetVar, "\""]},
	 {r, [?TAB,"$SUDO \"mkdir -p ", TargetEtc, "\""]},
	 {r, [?TAB,"$SUDO \"chown $XUSER:$XUSER ", TargetVar, "\""]},
	 {r, [?TAB,"$SUDO \"chown $XUSER:$XUSER ", TargetEtc, "\""]},
	 {r, ["fi"]},
	 %% check for busy box and set cp arguments
	 {r, ["BUSYBOX=`readlink /bin/cp`"]},
	 {r, ["if [ \"$BUSYBOX\" = \"busybox\" ]; then"]},
	 {r, ["CPOPTS=\"-RPpf\""]},  %% we need to force copy here or fail!
	 {r, ["else"]},
	 {r, ["CPOPTS=\"-RPpn\""]},
	 {r, ["fi"]},
	 %% copy var and etc structures
	 {r,["$NSUDO \"cp $CPOPTS ",
	     filename:join(Var,"*"), " ", TargetVar, "\""]},
	 {r,["$NSUDO \"cp $CPOPTS ",
	     filename:join(Etc,"*"), " ", TargetEtc, "\""]}] ++

	Configs ++
	
	[{r,["$NSUDO \"cp -p ", filename:join(TargetEtcRel,AppName++".run")," ", filename:join(TargetEtc, AppName++".run"),"\""]},
	 {r, ["$NSUDO \"cp -p ", 
	      filename:join(TargetEtcRel,"release"), " ",
	      filename:join(TargetEtc,"release"), "\""]}
	],
    Script1 = nl({script,Script0}),
    Script2 = flat(Script1),
    Filename = filename:join(installation_root_dir(AppName,Rel),"install.sh"),
    ok = file:write_file(Filename, list_to_binary(Script2)),
    ok = make_executable(Filename),
    ok.

make_osx_info_plist(AppName,Rel) ->
    Script1 = info_plist(AppName,Rel),
    Script2 = nl(Script1),
    Script3 = flat(Script2),
    PList = filename:join([installation_root_dir(AppName,Rel),
			   "Contents","Info.plist"]),
    ok = file:write_file(PList, list_to_binary(Script3)),
    ?dbg("wrote file: ~s\n", [PList]),
    ok.

info_plist(AppName,Rel) ->
    App = string:titlecase(AppName),
    Script =
[
 {r, ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"]},
 {r, ["<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"]},
 {r, ["<plist version=\"1.0\">"]},
 {r, [?TAB,"<dict>"]},
 {r, [?TAB,?TAB, "<key>CFBundleExecutable</key>"]},
 {r, [?TAB,?TAB, "<string>", App, "</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundleSignature</key>"]},
 {r, [?TAB,?TAB, "<string>", string:uppercase(App),"</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundlePackageType</key>"]},
 {r, [?TAB,?TAB, "<string>APPL</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundleVersion</key>"]},
 {r, [?TAB,?TAB, "<string>", Rel, "</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundleIdentifier</key>"]},
 {r, [?TAB,?TAB, "<string>org.erlang.apps."++AppName++"</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundleDisplayName</key>"]},
 {r, [?TAB,?TAB, "<string>",App,"</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundleName</key>"]},
 {r, [?TAB,?TAB, "<string>",App,"</string>"]},
 {r, [?TAB,?TAB, "<key>CFBundleIconFile</key>"]},
 {r, [?TAB,?TAB, "<string>AppIcon.icns</string>"]},
 {r, [?TAB, "</dict>"]},
 {r, ["</plist>"]}],
    {script,Script}.

%% Make the serv plist to use with mac os, replace the start/stop
%% sudo launchctl load /etc/erlang/<app>/<app>.plist 
%% sudo launchctl unload /etc/erlang/<app>/<app>.plist 

make_osx_plist(AppName,Rel) ->
    Script1 = osx_plist(AppName,Rel),
    Script2 = nl(Script1),
    Script3 = flat(Script2),
    Filename = "org.erlang."++to_string(AppName)++".plist",
    Etc = installation_etc_dir(AppName,Rel,[AppName,Rel]),
    PList = filename:join(Etc, Filename),
    ok = file:write_file(PList, list_to_binary(Script3)),
    ?dbg("wrote file: ~s\n", [PList]),
    ok.

%% OSX:
%%   cp /etc/erlang/<app>/org.erlang.<app>.plist /System/Library/LaunchDaemons/
%%   sudo launchctl bootstrap system /System/Library/LaunchDaemons/org.erlang.<app>.plist
%%   sudo launchctl kickstart system/org.erlang.<app>.plist
%%   sudo launchctl enable system/org.erlang.<app>.plist
%%
osx_plist(AppName,Rel) ->
    Var = filename:join(?TARGET_VAR++[AppName]),
    EtcDir = filename:join(?TARGET_ETC++[AppName]),
    RelDir = filename:join([Var,"rel",Rel]),

    PatchDir = filename:join([RelDir,"lib","PATCHES","ebin"]),
    ArgsFilePath = filename:join([EtcDir,Rel,"start.args"]),
    ErlPath  = filename:join([RelDir,"bin","erl"]),
    User = os:getenv("USER"),

    Configs = 
	lists:append(
	  [[{r, [?TAB,?TAB,"<string>-config</string>"]},
	    {r, [?TAB,?TAB,"<string>",filename:join(EtcDir,File),
		 "</string>"]}] ||
	      File <- get_config_filenames()]),

    Script = 
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
 {r, [?TAB,?TAB,?TAB,"<string>",EtcDir,"</string>"]},
 {r, [?TAB,?TAB,?TAB,"<key>ERL_CRASH_DUMP_SECONDS</key>"]},
 {r, [?TAB,?TAB,?TAB,"<string>0</string>"]},
 %%    {r, [?TAB,?TAB,?TAB,"<key>DYLD_LIBRARY_PATH</key>"]},
 %%    {r, [?TAB,?TAB,?TAB,"<string>/opt/local/lib:</string>"]},
 {r, [?TAB,?TAB,"</dict>"]},
 {r, [?TAB,?TAB,"<key>ProgramArguments</key>"]},
 {r, [?TAB,?TAB,"<array>"]},
 {r, [?TAB,?TAB,"<string>", ErlPath, "</string>"]}] ++

	Configs ++
[
 {r, [?TAB,?TAB,"<string>-pa</string>"]},
 {r, [?TAB,?TAB,"<string>",PatchDir,"</string>"]},
 {r, [?TAB,?TAB,"<string>-noinput</string>"]},  %% detached does not work!!
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
 {r, [?TAB,?TAB,"<string>", Var, "</string>"]},
 {r, [?TAB,"</dict>"]},
 {r, ["</plist>"]}],

    {script,Script}.

%%
%% Linux: init.d script
%% Install:
%%        cp etc/init.d/<app>  to /etc/init.d/
%%        chmod +x /etc/init.d/<app>
%%        sudo update-rc.d <app> defaults
%%
make_init_d(AppName,Rel) ->
    Initd = filename:join([installation_root_dir(AppName,Rel),"etc","init.d"]),
    ok = make_dir(Initd),
    Script1 = init_d(AppName),
    Script2 = nl(Script1),
    Script3 = flat(Script2),
    Filename = filename:join([Initd, to_string(AppName)]),
    ok = file:write_file(Filename, list_to_binary(Script3)),
    ok = make_executable(Filename),
    ?dbg("wrote file: ~s\n", [Filename]),
    ok.

init_d(AppName) ->
    {script,
     [
      {r, ["#!/bin/sh"]},
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
      {r, ["    interactive)"]},
      {r, ["	$DAEMON interactive"]},
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
      {r, ["	echo \"Usage: $N {start|stop|status|interactive|restart|reload|force-reload}\" >&2"]},
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
	{unix,_} -> "su - " ++ User  ++ " -c ";
	{win32,nt} -> ""
    end.

%% Argument to set node name
%% -sname x
%% -name x
erl_node_arg() ->
    Node = atom_to_list(node()),
    case string_split(Node, $@) of
	["nonode", "nohost"] -> [];
	[Name,Host] ->
	    case string_split(Host, $.) of
		["localhost"] -> [{"sname",Node}];  %% keep short hostname
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
make_cookie_file(AppName,Rel) ->
    case erlang:get_cookie() of
	nocookie -> 
	    {error,nocookie};
	Cookie ->
	    CookieFile = installation_etc_dir(AppName,Rel,
					      [AppName,".erlang.cookie"]),
	    CookieData = list_to_binary(atom_to_list(Cookie)),
	    ?dbg("write file: ~s\n", [CookieFile]),
	    file:write_file(CookieFile, CookieData),
	    {ok,Info} = file:read_file_info(CookieFile),
	    Mode = Info#file_info.mode band 8#700,
	    file:change_mode(CookieFile, Mode)
    end.
    
%% -hidden - make sure client node is not attached into a cluster
erl_hidden_arg() ->
    [{"hidden", ""}].

erl_config_flags(AppName) ->
    erl_config_flags(AppName,[]).

erl_config_flags(_AppName,DirList) ->
    [ " -config "++ filename:join(DirList++[File]) || 
	File <- get_target_config_files()].

erl_smp_arg() ->
    case erlang:system_info(smp_support) of
	true -> [{"smp", "enabled"}];
	false -> []
    end.

erl_noshell_arg() ->
    case init:get_argument(noshell) of
	{ok,[[]]} ->
	    [{"noshell",""}];
	_ ->
	    []
    end.

erl_noinput_arg() ->
    case init:get_argument(noinput) of
	{ok,[[]]} ->
	    [{"noinput",""}];
	_ ->
	    []
    end.

%% -detached is not present in the argument list for
%% some reason, so we use -wx | -epx when we want to start
%% application that are detached
erl_detached_arg() ->
    case init:get_argument(wx) of
	{ok,[[]]} ->
	    [{"detached",""}];
	_ ->
	    case init:get_argument(epx) of
		{ok,[[]]} ->
		    [{"detached",""}];
		_ ->	
		    []
	    end
    end.

%% ignore stdout and stderr
erl_ignore_stdout() ->
    case init:get_argument(noshell) of
	{ok,[[]]} ->
	    " > /dev/null 2>&1";
	_ ->
	    ""
    end.

%%
%% here we select either:
%%     -s M | -s M F | -s M F A1 .. An |
%%     -s AppName
%% or
%%     -eval "application:ensure_all_started(AppName)"  (R18)
%%
erl_start_arg(AppName) ->
    case get_started_args() of
	[] ->
	    [{"s", AppName}];  %% fallback
	Args0 ->
	    io:format("started args = ~w\n", [Args0]),
	    Args = rewrite_started_args(Args0),
	    io:format("new started args = ~w\n", [Args]),
	    [{"s", As} || As <- Args ]
    end.

rewrite_started_args([S=[servator,make_release,servator|_]|As]) ->
    %% make release of servator it self...
    [S | rewrite_started_args(As)];
rewrite_started_args([_S=[servator,_|_]|As]) ->
    %% make release of application, remove start of servator
    rewrite_started_args(As);
rewrite_started_args([_S=[erlang,halt|_]|As]) ->
    %% remove halt, used in make files, not needed at runtime?
    rewrite_started_args(As);
rewrite_started_args([[Mod,start0|Args]|As]) ->
    %% dummy start function to prepare for application but no real start!
    [[Mod,start|Args] | rewrite_started_args(As)];
rewrite_started_args([S|As]) ->
    [S | rewrite_started_args(As)];
rewrite_started_args([]) ->
    [].

erl_stop_arg(_AppName) ->
    NodeName = atom_to_list(node()),
    [{"eval", "case rpc:call('"++NodeName++"', init, stop, []) of ok -> erlang:display(ok), erlang:halt(0); {badrpc,R} -> erlang:display(R), erlang:halt(1) end."}].

%% status check code:
%%  case net_adm:pin(Node) of
%%     pong ->
%%        try rpc:call(Node,App,status,[])
%%        catch error:_ -> erlang:display(up)
%%        end,
%%        erlang:halt(0);
%%     pang ->
%%        erlang:display(down),
%%        erlang:halt(1)
%%  end
%%
erl_status_arg(AppName) ->
    NodeName = atom_to_list(node()),
    [{"eval", "case net_adm:ping('"++NodeName++"') of pong -> try rpc:call('"++NodeName++"',"++AppName++",status,[]) catch error:_ -> erlang:display(up) end, erlang:halt(0); pang -> erlang:display(down), erlang:halt(1) end."}].

erl_attach_arg(_AppName) ->
    NodeName = atom_to_list(node()),
    [{"remsh", NodeName}].

%%
%% Heart commands
%%
erl_heart_arg(AppName) ->
    case init:get_argument(heart) of
	{ok, [""]} ->
	    %% The return value I've seen so far
	    erl_heart_arg_cont(AppName);
	{ok, _Args} ->
	    io:format("Warning, don't know how to handle heart ~p", [_Args]),
	    erl_heart_arg_cont(AppName);
	error ->
	    []
    end.

erl_heart_arg_cont(AppName) ->
    [{env, "HEART_COMMAND", 
	  filename:join(?TARGET_ETC ++ [AppName] ++ 
			    [to_string(AppName) ++ ".run"]) ++ 
	  " start"}] 
	++ 
	erl_heart_beat_timeout() 
	++ 
	[{heart,""}].

erl_heart_beat_timeout() ->
    case os:getenv("HEART_BEAT_TIMEOUT") of
	false ->
	    %% Use default 
	    [{env, "HEART_BEAT_TIMEOUT", ?HEART_BEAT_TIMEOUT}];
	HBT ->
	    try list_to_integer(HBT) of
		I ->
		    [{env, "HEART_BEAT_TIMEOUT", I}]
	    catch _:_ ->
		    io:format("illegal heart_beat_timeout ~p", [HBT]),
		    exit(failed)
	    end
    end.
	

make_args(start, AppName, _Rel) ->
    erl_node_arg() ++
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
	    io:format(Fd, "~s\n", [format_args(Args)]),
	    file:close(Fd),
	    ?dbg("wrote file: ~s\n", [File]),
	    ok;
	Error ->
	    io:format("error writing file: ~s: ~p\n", [File, Error]),
	    Error
    end.

format_args(Args) ->
    lists:map(
      fun ({"s", {eval,Value}}) ->
	      io_lib:format("-eval \"~s\" ", [Value]);
	  ({Opt,""}) ->
	      io_lib:format("-~s ", [Opt]);
	  ({Opt,Value}) when is_list(Value) ->
	      try erlang:iolist_size(Value) of
		  _N ->
		      io_lib:format("-~s \"~s\" ", [Opt,Value])
	      catch
		  error:_ ->
		      io_lib:format("-~s ~s ",
				    [Opt,format_list(Value)])
	      end;
	 ({Opt,Value}) when is_list(Value) ->
	      io_lib:format("-~s \"~s\" ", [Opt,Value]);
	 ({Opt,Value}) when is_atom(Value) ->
	      io_lib:format("-~s \"~s\" ", [Opt,Value]);
	 ({Opt,Value}) when is_integer(Value) ->
	      io_lib:format("-~s ~w ", [Opt,Value]);
	 ({env,Env,Value}) when is_integer(Value) ->
	      io_lib:format("-env ~s ~w ", [Env,Value]);
		({env,Env,Value}) when is_list(Value) ->
	      io_lib:format("-env ~s \"~s\" ", [Env,Value])
      end, Args).

format_list(List) ->
    lists:join(" ", format_elems(List)).

format_elems([A|As]) when is_atom(A) ->
    [io_lib:format("\"~s\"", [A]) | format_elems(As)];
format_elems([A|As]) when is_integer(A) ->
    [io_lib:format("~w", [A]) | format_elems(As)];
format_elems([A|As]) when is_float(A) ->
    [io_lib_format:fwrite_g(A) | format_elems(As)];
format_elems([A|As]) when is_list(A) ->
    [io_lib:format("\"~s\"", [A]) | format_elems(As)];
format_elems([]) ->
    [].

%% Generate the start command
shell_start_command(AppName,Rel,Home) ->
    User = os:getenv("USER"),
    Flags0 = "$OPTS " ++ erl_config_flags(AppName),
    Flags1 = Flags0 ++ " -pa $VAR/rel/$VSN/lib/PATCHES/ebin",
    Flags2 = Flags1 ++ " " ++ lists:flatten(format_args(erl_heart_arg(AppName))),
    Start = erl_args(AppName, start, Flags2++" -detached", Rel),
    DefaultDir = "$VAR",
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r,[?TAB, su_command(), ?Q, "(cd ", DefaultDir, "; ",
	  Home, "; ","export ERL_CRASH_DUMP_SECONDS=0; ",
	  backquote(Start), " > /dev/null 2>&1", ")", ?Q ]},
      {r,["else"]},
      {r,[?TAB, "(cd ", DefaultDir, "; ",
	  "export ERL_CRASH_DUMP_SECONDS=0; ",
	  Start," > /dev/null 2>&1", ")"]},
      {r,["fi"]}
     ]}.

%% Generate the interactive command
shell_interactive_command(AppName,Rel,Home) ->
    User = os:getenv("USER"),
    Flags0 = "$OPTS " ++ erl_config_flags(AppName),
    Flags1 = Flags0 ++ " -pa $VAR/rel/$VSN/lib/PATCHES/ebin",
    DefaultDir = "$VAR",
    Start = erl_args(AppName, start, Flags1, Rel),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r,[?TAB, su_command(), ?Q, "(cd ", DefaultDir, "; ",
	  Home,"; ",Start, ")", ?Q ]},
      {r,["else"]},
      {r,[?TAB, "(cd ", DefaultDir, "; ",
	  Start, ")"]},
      {r,["fi"]}
     ]}.

%% Generate the stop command
shell_stop_command(AppName,Rel,Home) ->
    User = os:getenv("USER"),
    NodeArg = client_node_arg(),
    Stop = erl_args(AppName, stop, NodeArg ++ " -noinput", Rel),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r, [?TAB, su_command(), ?Q, Home, "; ", Stop, ?Q]},
      {r, ["else"]},
      {r, [?TAB, Stop]},
      {r, ["fi"]}
     ]}.

%% Generate the attach command
shell_attach_command(AppName,Rel,Home) ->
    User = os:getenv("USER"),
    NodeArg = client_node_arg(),
    Attach = erl_args(AppName, attach, NodeArg, Rel),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r, [?TAB, su_command(), ?Q, Home, "; ", Attach, ?Q]},
      {r, ["else"]},
      {r, [?TAB, Attach]},
      {r, ["fi"]}
     ]}.

%% Generate the status command
shell_status_command(AppName,Rel,Home) ->
    User = os:getenv("USER"),
    NodeArg = client_node_arg(),
    Status = erl_args(AppName, status,  NodeArg ++ " -noinput", Rel),
    {script,
     [
      {r,["if [ ", ?Q, "$USER", ?Q,  " != ", ?Q, User, ?Q, " ]; then" ]},
      {r, [?TAB, su_command(), ?Q, Home, "; ", Status, ?Q]},
      {r, ["else"]},
      {r, [?TAB, Status]},
      {r, ["fi"]}
     ]}.

%% Generate the start command
shell_start_apprun(AppName,Rel,_Home,Interactive,BuildType) ->
    Flags0 = 
	if BuildType =:= starexec ->
		" -config $ETC/\$CONFIGFILE";
	   true ->
		erl_config_flags(AppName)
	end,
    Flags1 = Flags0 ++ " -pa $VAR/rel/$VSN/lib/PATCHES/ebin",
    Flags2 = Flags1 ++ " " ++ lists:flatten(format_args(erl_heart_arg(AppName))),
    Start = erl_args(AppName, start, Flags2, Rel),
    DefaultDir = "$VAR",
    Pipe = if Interactive -> 
		   "";
	      true ->
		   " > /dev/null 2>&1"
	   end,
    {script,
     [
      {r, ["(cd ", DefaultDir, "; ",
	   "export ERL_CRASH_DUMP_SECONDS=0; ",
	   Start, " -noshell", " -extra ", "\"\$@\"", Pipe, ")"]}
     ]}.

erl_args(AppName, Type, Flags, Rel) ->
    %% Args = init:get_arguments(),
    %% Name = proplists:get_value(progname, Args, "erl"),
    %% Executable = os:find_executable(Name),
    %% {ok,_Wd} = file:get_cwd(),
    Filename = to_string(Type)++".args",
    ArgsFilename = installation_etc_dir(AppName,Rel,[AppName,Rel,Filename]),
    emit_args_file(ArgsFilename, make_args(Type, AppName, Rel)),
    "$ERL " ++ Flags ++ " -args_file " ++
	filename:join(["$ETC","$VSN",Filename]).

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

%% magic to get -s flags from init process (may work sometimes :-)
get_started_args() ->
    {backtrace,Backtrace} = erlang:process_info(whereis(init), backtrace),
    Lines = binary:split(Backtrace, <<"\n">>, [global]),
    extract_(Lines).

extract_([<<"y(0)     []">>|Lines]) ->
    extract_(Lines);
extract_([<<"y(0)", TermData/binary>>|_Lines]) ->
    parse_state(TermData);
extract_([<<"y(1)", TermData/binary>>|_Lines]) ->
    parse_state(TermData);
%% OTP 23 
extract_([<<"0x",Tail/binary>> | Lines]) ->
    [_Hex, Data] = binary:split(Tail, <<" ">>),
    try parse_state(Data) of
	Start -> Start
    catch
	error:_ ->
	    extract_(Lines)
    end;
extract_([_Line|Lines]) ->
    extract_(Lines);
extract_([]) ->
    [].

parse_state(Data) ->
    {ok,Ts,_} = erl_scan:string(binary_to_list(Data)),
    Ts1 = translate_termdata(Ts),
    {ok,[Term]} = erl_parse:parse_exprs(Ts1++[{dot,1}]),
    State = erl_parse:normalise(Term),
    element(4, State).

%% translate scanned pids to placeholders
%% translate <x.y.z> (as tokens) into dummy 'pid'
translate_termdata([{'<',L},{float,_,_PidAsFloat},
		    {'.',_},{integer,_,_},{'>',_} |Ts]) ->
    [{atom,L,pid} | translate_termdata(Ts)];
%% translate <<ddd bytes>> into a empty binary
translate_termdata([{'<<',L},{integer,_,_DDD},{atom,_,bytes},{'>>',_}|Ts]) ->
    [{'<<',L},{'>>',L} | translate_termdata(Ts)];
translate_termdata([T|Ts]) ->
    [T|translate_termdata(Ts)];
translate_termdata([]) ->
    [].
    

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

copy_configs(AppName,Rel) ->
    case get_config_files() of
	[] ->
	    ok;
	ConfigFiles ->
	    DstFiles = [filename:basename(File) || File <- ConfigFiles],
	    check_config_files(DstFiles,AppName),
	    lists:foreach(
	      fun({SrcConfig,DstFile}) ->
		      case file:read_file(SrcConfig) of
			  {ok,Bin} ->
			      ConfigPath = installation_etc_dir(AppName,Rel,
								[AppName]),
			      ConfigFile = filename:join(ConfigPath,DstFile),
			      io:format("Config path = ~p\n", [ConfigFile]),
			      ok = file:write_file(ConfigFile, Bin),
			      ?dbg("copied file: ~s to ~s\n",
				   [SrcConfig, ConfigFile]),
			      RelPath = installation_etc_dir(AppName,Rel,
							     [AppName,Rel]),
			      RConfigFile = filename:join(RelPath,DstFile),
			      ok = file:write_file(RConfigFile, Bin),
			      ?dbg("copied file: ~s to ~s\n",
				   [SrcConfig, RelPath]),
			      ok;
			  Error ->
			      io:format("error copy file: ~s : ~p\n", 
					[SrcConfig, Error]),
			      Error
		      end
	      end, lists:zip(ConfigFiles, DstFiles))
    end.

get_target_config_files() ->
    TargetDir = case get_build_type() of
		    starexec -> "$ETC";
		    osxapp   -> "$PREFIX/Contents/Resources";
		    appimage -> "$PREFIX";
		    win32app -> "$PREFIX";
		    _ -> "$ETC"
		end,
    [filename:join(TargetDir,filename:basename(File)) || 
	File <- get_config_files()].

get_config_filenames() ->
    [filename:basename(File) || File <- get_config_files()].

get_config_files() ->
    Args = init:get_arguments(),
    case proplists:get_all_values(config, Args) of
	[] -> [];
	Configs ->
	    [case filename:extension(File) of
		 "" -> File++".config";
		 ".config" -> File;
		 _ -> File
	     end || [File] <- Configs]
    end.

%% check that:
%% - config files has unique basenames  (error)
%% - <app>.config or sys.config are present (warn)
check_config_files(Files, AppName) ->
    check_config_files(Files),
    case lists:member(AppName++".config", Files) orelse
	lists:member("sys.config", Files) of
	false ->
	    io:format("warning: missing <app>.config or sys.config file\n");
	true ->
	    ok
    end.

check_config_files([File|Files]) ->
    case lists:member(File, Files) of
	true ->
	    io:format("error: ~s has a duplicate\n", [File]),
	    erlang:error({duplicate_config,File});
	false ->
	    ok
    end,
    check_config_files(Files);
check_config_files([]) ->
    ok.
    
%%
%% Copy erts data
%%   erts-<vsn>/bin
%%       beam | beam.smp, child_setup, epmd, heart, inet_gethost,
%%       erlexec, escript
%%
%%  vsn = erlang:system_info(version)
%%  read root from init:get_arguments()
%%
copy_erlang_erts(AppName,Rel) ->
    {Type,Exe,Dll} = 
	case os:type() of
	    {win32,nt} -> {win32,".exe", ".dll"};
	    {T,_} -> {T, "", ""}
	end,
    Beam = case erlang:system_info(smp_support) of
	       true -> "beam.smp"++Dll;
	       false -> "beam"++Dll
	   end,
    Args = init:get_arguments(),
    [Root] = proplists:get_value(root, Args),
    ErtsVsn = "erts-"++erlang:system_info(version),
    SrcDir = filename:join([Root, ErtsVsn, "bin"]),
    DstDir = installation_var_dir(AppName,Rel,
				  [AppName, ErtsVsn, "bin"]),
    ok = make_dir(DstDir),

    ChildSetup = 
	if Type =:= win32 ->
		[];
	   true ->
		OtpRelease = list_to_integer(erlang:system_info(otp_release)),
		if OtpRelease >= 19 ->
			["erl_child_setup"];
		   true ->
			["child_setup"]
		end
	end,
    Werl = 
	if Type =:= win32 ->
		["werl.exe"];
	   true ->
		[]
	end,
    lists:foreach(
      fun(File) ->
	      copy_with_mode(filename:join([SrcDir, File]),
			     filename:join([DstDir, File]))
      end, [Beam] ++ ChildSetup ++ Werl ++
	  ["epmd"++Exe, 
	   "heart"++Exe, 
	   "inet_gethost"++Exe, 
	   "erlexec"++Dll, 
	   "escript"++Exe]).

%% get all applications that App depend on
depend_applications(AppName) when is_list(AppName) ->
    depend_applications(list_to_atom(AppName));
depend_applications(App) when is_atom(App) ->
    depend_applications_(applications(App), []).

depend_applications_([App|Apps], Acc) ->
    case lists:member(App, Acc) of
	true ->
	    depend_applications_(Apps, Acc);
	false ->
	    Ds = applications(App),
	    depend_applications_(Apps++Ds, [App|Acc])
    end;
depend_applications_([], Acc) ->
    Acc.

applications(App) ->
    case application:get_key(App, applications) of
	undefined -> [];
	{ok,Deps} -> Deps
    end.

%% get all applications that App depend on,
%% but filter out all system applications 
depend_user_applications(App) ->
    depend_applications(App) -- system_applications().

user_applications() ->
    user_applications_([]).

user_applications(AppName) ->
    user_applications_(depend_user_applications(AppName)).

%% some known auto loaded applications (from .erlang )
auto_loaded_application(fnotify) -> true;
auto_loaded_application(error_emacs) -> true;
auto_loaded_application(_) ->  false.
    
user_applications_(AppUserDepend) ->
    SysApps = system_applications(),
    %% special treat on some loaded "known applications"
    UserApps = 
	lists:filter(
	  fun({App,_Comment,_Vsn}) ->
		  case lists:member(App,AppUserDepend) of
		      true -> true;
		      false ->
			  case lists:member(App, SysApps) of
			      true -> false;
			      false -> not auto_loaded_application(App)
			  end
		  end
	  end, application:loaded_applications()),

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
    
copy_user_applications(AppName,Rel) ->
    DstDir = installation_var_dir(AppName,Rel,[AppName, "lib"]),
    ok = make_dir(DstDir),
    lists:foreach(
      fun({App,_Descr,Vsn}) ->
	      Src = code:lib_dir(App),
	      Dst = filename:join(DstDir, atom_to_list(App)++"-"++Vsn),
	      copy_app(Src, Dst)
      end, user_applications(AppName)).

otp_applications() ->
    SysApps = system_applications(),
    lists:filter(fun({App,_Comment,_Vsn}) -> lists:member(App, SysApps) end,
		 application:loaded_applications()).

copy_otp_applications(AppName,Rel) ->
    DstDir = installation_var_dir(AppName,Rel,[AppName, "lib"]),
    ok = make_dir(DstDir),
    lists:foreach(
      fun({App,_Descr,Vsn}) ->
	      Src = code:lib_dir(App),
	      Dst = filename:join(DstDir, atom_to_list(App)++"-"++Vsn),
	      copy_app(Src, Dst)
      end, otp_applications()).

copy_erts_scripts(AppName,Rel) ->
    {Type,Exe,_Dll} = 
	case os:type() of
	    {win32,nt} -> {win32,".exe", ".dll"};
	    {T,_} -> {T, "", ""}
	end,
    RelDir = installation_var_dir(AppName,Rel,[AppName, "rel", Rel]),
    Args = init:get_arguments(),
    [Root] = proplists:get_value(root, Args),
    SrcDir = filename:join(Root, "bin"),
    BinDir = case get_build_type() of
		 appimage -> RelDir;
		 osxapp -> RelDir;
		 win32app -> RelDir;
		 _ -> 
		     filename:join(RelDir, "bin")
	     end,
    %% copy scripts
    CondScript = if Type =:= win32 -> ["erl"++Exe];
		    true -> ["start.script"]
		 end,
    lists:foreach(
      fun(File) ->
	      copy_with_mode(filename:join([SrcDir, File]),
			     filename:join([BinDir, File]))
      end, ["erlc"++Exe, "escript"++Exe, "start.boot"]++CondScript),

    if Type =/= win32 ->
	    %% copy "erl" and patch location
	    %% ROOTDIR=filename:join("/", RelDir),
	    ROOTDIR="$THISDIR",
	    BINDIR = case get_build_type() of
			 appimage -> "$ROOTDIR/bin";
			 osxapp   -> "$ROOTDIR/bin";
			 _ -> "$ROOTDIR/erts/bin"
		     end,
	    THISDIR="THISDIR=`dirname \"$0\"`\nTHISDIR=`(cd \"$THISDIR/..\" \\&\\& pwd)`",
	    copy_replace(filename:join(SrcDir, "erl"),
			 filename:join(BinDir, "erl"),
			 [{"ROOTDIR=.*", THISDIR++"\n"++"ROOTDIR=\""++ROOTDIR++"\"",[]},
			  {"BINDIR=.*", "BINDIR="++BINDIR, []}
			 ]);
       true ->
	    ok
    end.

make_release_dir(AppName, Rel) ->
    case get_build_type() of
	appimage -> copy_erts_scripts(AppName,Rel);
	osxapp -> copy_erts_scripts(AppName,Rel);
	win32app -> copy_erts_scripts(AppName,Rel);
	_ -> make_release_dir_(AppName,Rel)
    end.


make_release_dir_(AppName, Rel) ->
    RelDir = installation_var_dir(AppName,Rel,[AppName, "rel", Rel]),
    RelLibDir = filename:join(RelDir, "lib"),
    ok = make_dir(RelLibDir),
    %% create a patches directory that is ( always search first )
    ok = make_dir(filename:join([RelLibDir, "PATCHES", "ebin"])),
    
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
      end, user_applications(AppName)),
    
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
    ok = make_dir(BinDir),

    %% copy erlc, escript, start.boot, start.script, erl
    copy_erts_scripts(AppName,Rel),

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
	{ok, _Link} -> ok;
	{error,eexist} ->
	    case file:read_link(New) of
		{ok, _Link} -> ok;
		Error -> Error
	    end;
	Error -> Error
    end.
		    
%% copy application directory
%% ebin priv include  [ src ]
%%

%% default to "binary" release
copy_app(Src, Dst) ->
    copy_app(Src, Dst, ["ebin", "priv", "include"]).

copy_app(Src, Dst, DirList) ->
    ok = make_dir(Dst),
    lists:foreach(
      fun(File) ->
	      SrcDir = filename:join(Src, File),
	      case filelib:is_dir(SrcDir) of
		  true ->
		      DstDir = filename:join(Dst,File),
		      copy_dir(SrcDir, DstDir);
		  false ->
		      ok
	      end
      end, DirList),
    ?dbg("copied app ~s\n", [Src]),
    ok.


copy_dir(Src, Dst) ->
    ok = make_dir(Dst),
    case file:list_dir(Src) of
	{ok,[]} -> ok;
	{ok,Files} ->
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
	    ?dbg("copied dir ~s to ~s\n", [Src, Dst]);
	Error ->
	    Error
    end.

copy_with_mode(Src, Dst) ->
    ?dbg("copy file ~s to ~s\n", [Src, Dst]),
    {ok,_Size} = file:copy(Src, Dst),
    ?dbg("copied [~w bytes]\n", [_Size]),
    {ok,Info} = file:read_file_info(Src),
    file:change_mode(Dst, Info#file_info.mode).

%% Copy Src to Dst line by line and do regular expression replace 
%% while doing it.
copy_replace(Src, Dst, []) ->
    copy_with_mode(Src, Dst);
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

make_dir(Dir) ->
    Ds = filename:split(Dir),
    make_dir_(Ds, ".").

make_dir_([Dir|Ds], Path) ->
    Path1 = filename:join(Path, Dir),
    ?dbg("make_dir ~s\n", [Path1]),
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

backquote([?Q|Cs]) ->
    [?BSLASH,?Q|backquote(Cs)];
backquote([C|Cs]) ->
    [C|backquote(Cs)];
backquote([]) ->
    [].
