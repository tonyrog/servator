# Servator

A service creator based on the state of the running system. The idea is
to inspect a running system and create start scripts based on what is
found running.
It is so simple to create server scripts this way, example

    $ erl -sname foo -s foo -config foo.config
    > servator:make_soft_release(foo).
    > ...
    > halt().
    
    $ mv etc/erlang/foo /etc/erlang/foo
    $ mv var/erlang/foo /var/erlang/foo
    $ # maybe edit /etc/erlang/foo.config to reflect the new home

The soft release create a release structure but will use symbolic links
to refer to the libraries used.

    $ erl -sname foo -s foo -config foo.config
    > servator:make_release(foo, "1.0").
    > ...
    > halt().
    
    $ mv etc/erlang/foo /etc/erlang/foo
    $ mv var/erlang/foo /var/erlang/foo
    $ # maybe edit /etc/erlang/foo.config to reflect the new home

When creating a release all data is copied into a release structure
that can be use standalone.

More info about the release structure is found in RELEASE.md

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

    $ sudo cp etc/init.d/foo /etc/init.d/
    $ sudo chmod +x /etc/init.d/foo
    $ sudo update-rc.d foo defaults

## Linux rc.local

To start an erlang service on most linux system when system starts
add the following line to /etc/rc.local ( before any exit 0 )

    # start <app> erlang service
    /etc/erlang/<app>/<app>.run start

## Mac OS X

On 10.10 ( and later ? )

    sudo cp /etc/erlang/<app>/org.erlang.<app>.plist /System/Library/LaunchDaemons/
    sudo launchctl bootstrap system /System/Library/LaunchDaemons/org.erlang.<app>.plist
    sudo launchctl kickstart system/org.erlang.<app>.plist
    sudo launchctl enable system/org.erlang.<app>.plist

On 10.x before 10.10 ( still works in 10.10! )

    sudo cp /etc/erlang/<app>/org.erlang.<app>.plist /System/Library/LaunchDaemons/
    sudo launchctl load -w /System/Library/LaunchDaemons/org.erlang.<app>.plist

Print current status (10.10 ?)

    launchctl print system/org.erlang.xylan


10.11 (El Capitan)

In 10.11 it is no longer possible to install stuff under /System/Library/LaunchDaemons so instead /Library/LaunchDaemons is used


    sudo cp /etc/erlang/<app>/org.erlang.<app>.plist /Library/LaunchDaemons/
    sudo launchctl load -w /Library/LaunchDaemons/org.erlang.<app>.plist
