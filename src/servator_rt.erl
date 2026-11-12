%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Runtime part of servator reponsible for upgrades etc
%%% @end
%%% Created : 10 Sep 2016 by Tony Rogvall <tony@rogvall.se>

-module(servator_rt).

-export([compare_version/2]).
-export([parse_version/1]).
-export([diff/2]).
-export([diff_apps/3]).
-export([apps_versions/1]).
-export([app_version/2]).
-export([split_version/1]).
-export([parse_package/1]).
-export([parse_package_/2]).
-export([parse_architecture/1]).
-export([parse_name/1]).
-export([parse_name/2]).
-export([trim_string/1]).
-export([user_dir/1, user_file/2]).
-export([user_config_file/1, install_user_config/1]).
-export([load_user_config/1, load_config_file/1, merge_config/1]).
-export([merge/2]).
-export([installed_version/1, set_installed_version/2]).
-export([install_log/2]).

-define(is_digit(X), (((X) >= $0) andalso ((X) =< $9))).
-define(is_lower(X), (((X) >= $a) andalso ((X) =< $z))).
-define(is_upper(X), (((X) >= $A) andalso ((X) =< $Z))).
-define(is_alpha(X), (?is_lower((X)) orelse ?is_upper((X)))).
-define(is_alnum(X), (?is_alpha((X)) orelse ?is_digit((X)))).

%%
%% Generate a list of new and old apps and files
%%
diff(NewLib, OldLib) ->
    New = apps_versions(NewLib),
    NewApps  = [App || {App,_Vsn} <- New],
    Old = apps_versions(OldLib),
    OldApps = [App || {App,_Vsn} <- Old],
    %% apps that are present in both
    InterApps = NewApps -- (NewApps -- OldApps),
    Update = 
	[ begin
	      NewVsn = proplists:get_value(App,New),
	      OldVsn = proplists:get_value(App,Old),
	      case compare_version(NewVsn,OldVsn) of
		  1 -> {upgrade, {App,NewVsn,OldVsn}};
		  0 -> {keep, App};
		  -1 -> {downgrade,  {App,NewVsn,OldVsn}}
	      end
	  end || App <- InterApps ],
    Install = 
	[ begin
	      Vsn = proplists:get_value(App,New),
	      {install, {App,Vsn}}
	  end || App <- (NewApps -- InterApps) ],
    UnInstall =
	[ begin
	      Vsn = proplists:get_value(App,Old),
	      {uninstall, {App,Vsn}}
	  end || App <- (OldApps -- InterApps) ],
    Install ++ Update ++ UnInstall.

diff_apps(NewLib,OldLib,[App|Apps]) ->
    NewAppVsn = app_version(NewLib, App),
    OldAppVsn = app_version(OldLib, App),
    {ok,NewMods} = file:list_dir(filename:join([NewLib,App,"ebin"])),
    {ok,OldMods} = file:list_dir(filename:join([OldLib,App,"ebin"])),
    [{NewAppVsn,OldAppVsn,NewMods,OldMods}|
     diff_apps(NewLib,OldLib,Apps)];
diff_apps(_NewLib,_OldLib,[]) ->
    [].

%% Read all applications in directory Lib
apps_versions(Lib) ->
    {ok,Apps} = file:list_dir(Lib),
    [ app_version(Lib,App) || App <- Apps].

%%
%% Given Library directory and app directory name try to determine
%% The application version. 1. Read app file, 2. extract from name
%%
app_version(Lib, App) ->
    AppDir = filename:join(Lib,App),
    case filelib:wildcard(filename:join([AppDir,"ebin","*.app"])) of
	[AppFile] ->
	    {ok,[{application,AppName,Conf}]} = file:consult(AppFile),
	    Version = proplists:get_value(vsn, Conf, ""),
	    {Vsn,_} =  parse_version(Version),
	    {AppName,Vsn};
	_ ->
	    case file:read_link(AppDir) of
		{ok,Link} ->
		    split_version(filename:basename(Link));
		{error,_} ->
		    split_version(App)
	    end
    end.

%% split version strings like "foo-1.2" -> {'foo', {0,"1.2",""}}
split_version(App) ->
    case string:chr(App,$-) of
	0 -> 
	    {list_to_atom(App), {0,"1",""}};
	N ->
	    AppName = list_to_atom(string:substr(App,1,N-1)),
	    {Vsn,[]} = parse_version(string:substr(App, N+1)),
	    {AppName,{0,Vsn,""}}
    end.

%% Parse package names and versions

%%
%% <package> = [0-9a-zA-Z][0-9a-zA-Z+-.] (length >= 2)
%% return {Package, Rest}
%% or {error, Reason}
%%
parse_package([X1,X2|Xs]) when 
      ?is_alnum(X1), (?is_alnum(X2) 
		      orelse (X2 =:= $+)
		      orelse (X2 =:= $-)
		      orelse (X2 =:= $.)) ->
    parse_package_(Xs, [X2,X1]);
parse_package(_Xs) ->
    {error,no_package}.

parse_package_(Cs0=[C|Cs], Acc) ->
    if ?is_alnum(C) -> parse_package_(Cs, [C|Acc]);
       C =:= $+;
       C =:= $-;
       C =:= $. -> parse_package_(Cs, [C|Acc]);
       true ->
	    {lists:reverse(Acc), Cs0}
    end;
parse_package_([],Acc) ->
    {lists:reverse(Acc), ""}.


%%
%% <architecture> = <os>'-'<arch> | <arch> = {Os,Arch}
%%
parse_architecture(Cs) ->
    case parse_name(Cs) of
	Err={error,_} -> Err;
	{Nm, [$-|Cs1]} ->
	    case parse_name(Cs1) of
		Err={error,_} -> Err;
		{Nm1, Cs2} -> {{Nm,Nm1},Cs2}
	    end;
	{Nm,Cs1} ->
	    {{any,Nm}, Cs1}
    end.

parse_name([C|Cs]) when ?is_alnum(C) ->
    parse_name(Cs, [C]);
parse_name(_) ->
    {error, no_name}.

parse_name(Cs0=[C|Cs], Acc) ->
    if ?is_alnum(C) -> parse_name(Cs, [C|Acc]);
       true -> {lists:reverse(Acc),Cs0}
    end;
parse_name([],Acc) ->
    {lists:reverse(Acc),""}.


%%
%% <version> = [<epoch>:]<upstream_version>[-<debian_revision>]
%% <epoch> = [0-9]+
%% <upstream_version> = [0-9a-zA-Z][0-9a-zA-Z.+-:~]*  (CHANGED!)
%% <debian_revision> = [0-9a-zA-Z.+~]
%%
%% return {{Epoch,Version,Revision}, Rest}
%% or {error,Reason}
%%
parse_version(String) ->
    case string:to_integer(String) of
	{error, no_integer} ->
	    parse_version(0, String);
	{Epoch,[$:|String1]} ->
	    parse_version(Epoch, String1);
	{_, _Rest} ->
	    parse_version(0, String)
    end.


parse_version(Epoch, String) ->
    case string:rchr(String, $-) of
	0 ->
	    case parse_upstream_version(String) of
		Err = {error,_} -> Err;
		{Vsn, Rest} -> {{Epoch,Vsn,""}, Rest}
	    end;
	I ->
	    {String1,[$-|String2]} = lists:split(I-1, String),
	    case parse_upstream_version(String1) of
		Err = {error,_} -> Err;
		{Vsn, ""} ->
		    case parse_revision(String2) of
			Err = {error,_} -> Err;
			{Rev,Rest1} ->
			    {{Epoch,Vsn,Rev}, Rest1}
		    end;
		{_Vsn,_} ->
		    {error, no_version}
	    end
    end.
	    
%%
%% <upstream_version = [0-9][0-9a-zA-Z.+-:~]*
%% Change to [0-9a-zA-Z][0-9a-zA-Z.+-:~]*
parse_upstream_version([X|Xs]) when ?is_alnum(X) ->
    parse_upstream_version(Xs, [X]);
parse_upstream_version(_) -> {error,no_upstream_version}.

parse_upstream_version(Xs0=[X|Xs],Acc) ->
    if ?is_alnum(X) -> parse_upstream_version(Xs,[X|Acc]);
       X =:= $.; X =:= $+;
       X =:= $-; X =:= $:;
       X =:= $~ -> parse_upstream_version(Xs,[X|Acc]);
       true -> {lists:reverse(Acc), Xs0}
    end;
parse_upstream_version([],Acc) ->
    {lists:reverse(Acc), ""}.

parse_revision(Xs) ->
    parse_revision(Xs,[]).

parse_revision(Xs0=[X|Xs], Acc) ->
    if ?is_alnum(X) -> parse_revision(Xs,[X|Acc]);
       X =:= $.; X =:= $+;
       X =:= $~ -> parse_revision(Xs);
       true -> {lists:reverse(Acc), Xs0}
    end;
parse_revision([], Acc) ->
    {lists:reverse(Acc), ""}.

trim_string(Cs) ->
    lists:reverse(trim_head(lists:reverse(trim_head(Cs)))).

trim_head([$\s|Cs]) -> trim_head(Cs);
trim_head([$\t|Cs]) -> trim_head(Cs);
trim_head([$\n|Cs]) -> trim_head(Cs);
trim_head([$\r|Cs]) -> trim_head(Cs);
trim_head(Cs) -> Cs.
    
%%
%% Compare To versions Vsn1 and Vsn2 
%% return +1  if Vs1 > Vsn2  (later)
%% reutrn  0  if Vs1 == Vsn2
%% return -1  if Vsn1 < Vsn2 (earlier)
%%

compare_version({Epoch1,Vsn1,Rev1},{Epoch2,Vsn2,Rev2}) ->
    if Epoch1 =:= Epoch2 ->
	    case cmpvsn(Vsn1, Vsn2) of
		0 ->
		    cmpvsn(Rev1, Rev2);
		C -> C
	    end;
       true ->
	    sign(Epoch1 - Epoch2)
    end.

cmpvsn([X|Xs], [Y|Ys]) when (not ?is_digit(X)), (not ?is_digit(Y)) ->
    if X =:= Y -> cmpvsn(Xs,Ys);
       X =:= $~ -> -1;
       Y =:= $~ -> 1;
       true -> sign(X-Y)
    end;
cmpvsn([X|_Xs],[Y|_Ys]) when not ?is_digit(X); not ?is_digit(Y) ->
    if 
	X =:= $~ -> -1;
	Y =:= $~ -> 1;
	?is_digit(X) -> -1;
	true -> 1
    end;
cmpvsn([X|Xs],[Y|Ys]) ->  %% digits
    {Xn,Xs1} = string:to_integer([X|Xs]),
    {Yn,Ys1} = string:to_integer([Y|Ys]),
    if Xn < Yn -> -1;
       Xn > Yn -> 1;
       true -> cmpvsn(Xs1,Ys1)
    end;
cmpvsn([], []) -> 0;
cmpvsn([X|_], []) -> 
    if X =:= $~ -> -1;
       true -> 1
    end;
cmpvsn([], [Y|_]) -> 
    if Y =:= $~ -> 1;
       true -> -1
    end.

sign(X) when X > 0 -> 1;
sign(X) when X < 0 -> -1;
sign(_X) -> 0.

%%====================================================================
%% Runtime support for applications built with servator
%%
%% User files are kept in $HOME/.<app>/
%%    user.config    -- user override config
%%    installed      -- installed version
%%    install_log.txt
%%
%% The user config is a normal OTP config file, but unlike -config
%% it is merged section by section into the application environment.
%% Only the keys given are changed, example: change one color
%%
%%   [{candy, [{profile, [{screen_color, black}]}]}].
%%
%% Merge rules:
%%   - key/value lists (proplists) are merged per key
%%     tuples {K,V} use K as key, larger tuples use all but the
%%     last element as key, {can_usb,1,Opts} has key {can_usb,1}
%%   - maps are merged per key
%%   - all other values (strings, numbers, plain lists) are replaced
%%====================================================================


-define(USER_CONFIG, "user.config").

home() ->
    case os:getenv("HOME") of
	false -> {ok,Cwd} = file:get_cwd(), Cwd;
	Home -> Home
    end.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(L) when is_list(L) -> L.

%% $HOME/.<app>
user_dir(App) ->
    filename:join(home(), [$.|to_list(App)]).

%% $HOME/.<app>/<name>
user_file(App, Name) ->
    filename:join(user_dir(App), Name).

user_config_file(App) ->
    user_file(App, ?USER_CONFIG).

%% old location created by AppRun: $HOME/.<app>.config
legacy_config_file(App) ->
    filename:join(home(), [$.|to_list(App)] ++ ".config").

%% create an empty user config file if not present
install_user_config(App) ->
    File = user_config_file(App),
    case filelib:is_regular(File) of
	true -> ok;
	false ->
	    ok = filelib:ensure_dir(File),
	    file:write_file(File,
			    ["%% -*- erlang -*-\n"
			     "%% user override config for ", to_list(App), "\n"
			     "%% only the sections given are changed, example:\n"
			     "%% [{", to_list(App), ", [{profile, "
			     "[{screen_color, black}]}]}].\n"
			     "[\n"
			     "].\n"])
    end.

%% Load and merge user config files (legacy first)
%% may be called from command line: -s servator_rt load_user_config App
load_user_config([App]) when is_atom(App) ->
    load_user_config(App);
load_user_config(App) ->
    install_user_config(App),
    Files = [F || F <- [legacy_config_file(App), user_config_file(App)],
		  filelib:is_regular(F)],
    lists:foreach(fun load_config_file/1, Files).

load_config_file(File) ->
    case file:consult(File) of
	{ok, [Config]} when is_list(Config) ->
	    merge_config(Config);
	{ok, []} ->
	    ok;
	{ok, _} ->
	    logger:warning("~s: config must be a single list", [File]),
	    {error, badarg};
	{error, Reason} = Error ->
	    logger:warning("~s: ~s", [File, file:format_error(Reason)]),
	    Error
    end.

%% merge [{App,Env}] into the application environments
merge_config(Config) ->
    lists:foreach(
      fun({App, Env}) when is_atom(App), is_list(Env) ->
	      _ = application:load(App),
	      lists:foreach(
		fun(Elem) ->
			case kv(Elem) of
			    {Key, Value} when is_atom(Key) ->
				New = case application:get_env(App, Key) of
					  {ok, Old} -> merge(Old, Value);
					  undefined -> Value
				      end,
				application:set_env(App, Key, New,
						    [{persistent,true}]);
			    _ ->
				logger:warning("config ~w: bad element ~p",
					       [App, Elem])
			end
		end, Env);
	 (Bad) ->
	      logger:warning("config: bad element ~p", [Bad])
      end, Config).

%% Deep merge New into Old
merge(Old, New) when is_map(Old), is_map(New) ->
    maps:fold(fun(K, V, Acc) ->
		      case maps:find(K, Acc) of
			  {ok, V0} -> Acc#{ K => merge(V0, V) };
			  error -> Acc#{ K => V }
		      end
	      end, Old, New);
merge(Old, New) ->
    case is_kv_list(Old) andalso is_kv_list(New) of
	true -> merge_kv(Old, New);
	false -> New
    end.

merge_kv(Old, [Elem|New]) ->
    {Key, Value} = kv(Elem),
    merge_kv(merge_elem(Old, Key, Elem, Value), New);
merge_kv(Old, []) ->
    Old.

merge_elem([E|Es], Key, NewElem, NewValue) ->
    case kv(E) of
	{Key, OldValue} ->
	    [make_elem(NewElem, merge(OldValue, NewValue)) | Es];
	_ ->
	    [E | merge_elem(Es, Key, NewElem, NewValue)]
    end;
merge_elem([], _Key, NewElem, _NewValue) ->
    [NewElem].

%% split element into key and value
kv(A) when is_atom(A) -> {A, true};
kv({K, V}) -> {K, V};
kv(T) when is_tuple(T), tuple_size(T) > 2 ->
    [V|RKey] = lists:reverse(tuple_to_list(T)),
    {list_to_tuple(lists:reverse(RKey)), V};
kv(_) -> false.

make_elem(A, Value) when is_atom(A) -> {A, Value};
make_elem(T, Value) -> setelement(tuple_size(T), T, Value).

is_kv_list([]) -> false;
is_kv_list(L) when is_list(L) ->
    lists:all(fun(A) when is_atom(A) -> true;
		 (T) when is_tuple(T), tuple_size(T) >= 2 ->
		      is_atom(element(1, T));
		 (_) -> false
	      end, L);
is_kv_list(_) -> false.

%% installed version stored in $HOME/.<app>/installed
installed_version(App) ->
    case file:read_file(user_file(App, "installed")) of
	{ok, Bin} -> {ok, string:trim(binary_to_list(Bin))};
	Error -> Error
    end.

set_installed_version(App, Vsn) ->
    File = user_file(App, "installed"),
    ok = filelib:ensure_dir(File),
    file:write_file(File, [Vsn, "\n"]).

%% append lines to $HOME/.<app>/install_log.txt
install_log(App, Lines) ->
    File = user_file(App, "install_log.txt"),
    ok = filelib:ensure_dir(File),
    file:write_file(File, [[L, "\n"] || L <- Lines], [append]).
