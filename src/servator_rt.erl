%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Runtime part of servator reponsible for upgrades etc
%%% @end
%%% Created : 10 Sep 2016 by Tony Rogvall <tony@rogvall.se>

-module(servator_rt).
-compile(export_all).

-export([compare_version/2]).
-export([parse_version/1]).


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
