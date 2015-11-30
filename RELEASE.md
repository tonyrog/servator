
# Make a release

## full release

All otp/user applications are copied, with version, to this location:

    var/erlang/<app>/lib/..
	stdlib-1.0
	stdlib-1.1
	kernel-1.2
	app-1.3
	app-1.4 
	...

Erts are copied here:
    var/erlang/<app>/erts-vsn/..
	erts-5.9.1
	erts-7.0

    var/erlang/<app>/rel/..
	1.0/
	   erts -> ../../erts-5.9.1
	   lib/
		patches  ( local patches is a real directory )
		stdlib-1.18.1 -> ../../lib/stdlib-1.18.1
		kernel-2.15.1 -> ../../lib/kernel-2.15.1
		myapp  -> ../../lib/myapp-1.0
	   bin/
		epmd -> ../erts/epmd
		erl  ( ROOTDIR=/var/erlang/app/rel/1.0 )
		erlc
		escript
		start.boot
		start.script
		
	2.0/
	   erts -> ../erts-7.0
	   lib/
		patches ( local patches is a real directory )
		stdlib-2.5 -> ../../lib/stdlib-2.5/
		kernel-4.0 -> ../../lib/kernel-4.0/
		myapp  -> ../../lib/myapp-2.0
	   bin/
		epmd -> ../erts/epmd
		erl  ( ROOTDIR=/var/erlang/app/rel/2.0 )
		erlc
		escript
		start.boot
		start.script

## soft release for development

No applications are copied, symbolic links are setup:

    var/erlang/<app>/rel/..
	soft/
	   erts -> /usr/local/lib/erlang/erts-7.0
	   lib/
		stdlib-2.5 -> /usr/local/lib/erlang/lib/stdlib-2.5/
		kernel-4.0 -> /usr/local/lib/erlang/lib/kernel-4.0/
		myapp  -> /home/user/foo/erlang/myapp-2.0
	   bin/
		epmd -> ../erts/epmd
		erl  ( ROOTDIR=/var/erlang/app/rel/dev )
		erlc
		escript
		start.boot
		start.script

## Cookies

To be able to keep cookie safe, the cookie is stored at

    etc/erlang/<app>/erlang.cookie

in order to make this work HOME is also set to this directory

## Log files

Log files are located in var/erlang/<app>/log
