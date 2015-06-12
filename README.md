# Servator

A service creator based on the state of the running system. The idea is
to inspect a running system and create start scripts based on what is
found running.
It is so simple to create server scripts this way, example

    $ erl -sname foo -s foo -config foo.config
    > servator:make_linux_script(foo).
    > ...
    > halt().
    
    $ copy etc/erlang/foo to /etc/erlang/foo
    $ copy var/erlang/foo to /var/erlang/foo
    $ chmod +x /etc/erlang/foo/foo.run
    $ maybe edit /ets/erlang/foo.config to reflect the new home

The file layout created ( linux & mac ) is currently 

    /etc/erlang/<app>               ( config & script directory )
    /etc/erlang/<app>/start.args
    /etc/erlang/<app>/stop.args
    /etc/erlang/<app>/status.args
    /etc/erlang/<app>/attach.args
    /etc/erlang/<app>/<app>.run
    /etc/erlang/<app>/<config>     ( applications config )

Log files and state information is stored here

    /var/erlang/<app>              ( home direcory for app )
    /var/erlang/app/log            ( log files )
    /var/erlang/app/rel/...        ( for releases ? )
    /var/erlang/app/pid            ( running operating system pid )

