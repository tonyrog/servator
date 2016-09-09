
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

Erts is copied here:
    var/erlang/<app>/erts-vsn/..
	  erts-5.9.1
	  erts-7.0

    var/erlang/<app>/rel/..
	  1.0/
	   erts -> ../../erts-5.9.1
	   lib/
		PATCHES  ( local patches is a real directory )
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
		PATCHES ( local patches is a real directory )
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

## Config files

Configuration files are located under etc/erlang/<app>/<rel> for each
release.

    /etc/erlang/<app>/1.0
      start.args
      stop.args
      attach.args
      status.args
	  <app>.config
	  <app>.run
	  org.erlang.<app>.plist

If the <app>.config does not exist already it must be copied to
/etc/erlang/<app>/<app>.config. And it need to be updated if changed
during update, this is not done automatically.
The run file also need to be copied inorder to switch to the correct
version. Symbolic links are not always allowed so switch by copy is
recommended.

    cp /etc/erlang/<app>/1.1/<app>.run /etc/erlang/<app>/

To switch back to an older version just do:

    cp /etc/erlang/<app>/1.0/<app>.run /etc/erlang/<app>/

Do not forget to backup the old config before, this could be done
like this:

    cp /etc/erlang/<app>/<app>.config  /etc/erlang/<app>/1.0/<app>.config
    cp /etc/erlang/<app>/1.1/<app>.run /etc/erlang/<app>/

If a reverert to 1.0 is later needed then:

   cp /etc/erlang/<app>/1.0/<app>.config /etc/erlang/<app>/<app>.config
   cp /etc/erlang/<app>/1.0/<app>.run /etc/erlang/<app>/

## Cookies

To be able to keep cookies safe, cookies are in a file at location:

    etc/erlang/<app>/erlang.cookie

in order to make this work, the HOME environment variable is also set to
this directory.

## Log files

Log files are located in var/erlang/<app>/log
