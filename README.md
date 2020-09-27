# Servator

A service creator based on the state of the running system. The idea is
to inspect a running system and create start scripts based on what is
found running. Servator can also create applications for use with
AppImage Mac OS X Applications and Windows applications (see below)

# Normal release

It is so simple to create server scripts this way, example

    $ erl -sname foo -s foo -config foo.config
    > servator:make_release(foo).

    Or if a version not in application env must be used

    > servator:make_release(foo,"9.99").

    > ...
    > halt().

    $ cd foo-1.0; ./install.sh

NOTE. If /etc/erlang/foo or /var/erlang/foo does not exist or
are not owned by the current user. They must be created.

When creating a release all data is copied into a release structure
that can be use standalone.

# Soft release

    $ erl -sname foo -s foo -config foo.config
    > servator:make_soft_release(foo).
    > ...
    > halt().
    
    $ cd foo-soft; ./install.sh

You may have to edit /etc/erlang/foo/foo.config to reflect the new home

The soft release create a release structure but will use symbolic links
to refer to the libraries used.

More info about the release structure is found in RELEASE.md

# Make a release when using rebar deps

To build a release for an applications that use rebar deps the
following steps can be done:

    $ git clone git@github.com/user/foo
    $ cd foo
    $ unset REBAR_DEPS_DIR		# we just want deps dir
    $ unset ERL_LIBS			# ignore all other applications
    $ rebar get-deps
    $ rebar compile
    $ erl -sname foo -pa ../foo/ebin/ -pa deps/*/ebin -config foo.config -s foo
    > servator:make_release(foo, "1.01").

## Commands

Starting the service is as simple as

    /etc/erlang/<app>/<app>.run start

To stop then 

    /etc/erlang/<app>/<app>.run stop

Attach to the running node and get a shell

    /etc/erlang/<app>/<app>.run attach

ctrl-C will only kill the client node.

To get current status

    /etc/erlang/<app>/<app>.run status

will report 'down', 'up' or custom status if the function 
<app>:status() is implemented and generate some status output 
( safest is to use erlang:display to get some output )

Start in interactive mode

    /etc/erlang/<app>/<app>.run interactive

To start a node in "normal" mode can be nice when debuging.

## Linux init.d

    $ sudo cp foo-1.0/etc/init.d/foo /etc/init.d/
    $ sudo chmod +x /etc/init.d/foo
    $ sudo update-rc.d foo defaults

## Linux rc.local

To start an erlang service on most linux system when system starts
add the following line to /etc/rc.local ( before any exit 0 )

    # start <app> erlang service
    /etc/erlang/<app>/<app>.run start

## Mac OS X

On 10.10 ( and later ? )

    sudo cp /etc/erlang/<app>/<rel>/org.erlang.<app>.plist /System/Library/LaunchDaemons/
    sudo launchctl bootstrap system /System/Library/LaunchDaemons/org.erlang.<app>.plist
    sudo launchctl kickstart system/org.erlang.<app>.plist
    sudo launchctl enable system/org.erlang.<app>.plist

On 10.x before 10.10 ( still works in 10.10! )

    sudo cp /etc/erlang/<app>/<rel>/org.erlang.<app>.plist /System/Library/LaunchDaemons/
    sudo launchctl load -w /System/Library/LaunchDaemons/org.erlang.<app>.plist

Print current status (10.10 ?)

    launchctl print system/org.erlang.xylan


10.11 (El Capitan)

In 10.11 it is no longer possible to install stuff under /System/Library/LaunchDaemons so instead /Library/LaunchDaemons is used


    sudo cp /etc/erlang/<app>/<rel>/org.erlang.<app>.plist /Library/LaunchDaemons/
    sudo launchctl load -w /Library/LaunchDaemons/org.erlang.<app>.plist

# AppImage

	erl -noshell -s foo -s servator make_appimage foo -s erlang halt
	
This will create a directory called foo.AppDir with a AppRun file in it.
Now strip the erlang binaries and run appimage tool.

	strip foo.AppDir/bin/beam.smp
	strip foo.AppDir/bin/epmd
	strip foo.AppDir/bin/erlc
	strip foo.AppDir/bin/erl_child_setup
	strip foo.AppDir/bin/erlexec
	strip foo.AppDir/bin/escript
	strip foo.AppDir/bin/heart
	strip foo.AppDir/bin/inet_gethost
	appimagetool -n foo.AppDir

This should produce an executable that can be run.

# Max OS X Application

	erl -noshell -s foo -s servator make_osxapp foo -s erlang halt
	
	mkdir -p tmpdist
	rm -rf tmpdist/Foo.app
	mv Foo.app tmpdist/
	(cd tmpdist/; ../../servator/priv/make_icns ../priv/foo.png)
	rm -rf tmpdist/AppIcon.iconset
	mv tmpdist/AppIcon.icns tmpdist/foo.app/Contents/Resources/
	hdiutil create tmp.dmg -ov -volname "Foo" -fs HFS+ -srcfolder "./tmpdist/"
	hdiutil convert -format UDZO -o Foo.dmg tmp.dmg
	rm tmp.dmg

# Windows application


	erl -noshell -s foo -s servator make_win32app foo -s erlang halt

