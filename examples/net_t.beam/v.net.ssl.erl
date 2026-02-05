-module('v.net.ssl').
-export([new_ssl_conn/1]).

new_ssl_conn(Config) ->
    C = new_ssl_conn(maps:get(SSLConnectConfig, Config)),
    &#{SSLConn => C, {vbeam, type} => 'SSLConn'}.
