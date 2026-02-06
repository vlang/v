-module('v.net.ssl').
-export([new_ssl_dialer/1, 'SSLDialer.dial'/2, new_ssl_conn/1]).

new_ssl_dialer(Config) ->
    #{config => Config, {vbeam, type} => 'SSLDialer'}.

'SSLDialer.dial'(D, Address) ->
    new_ssl_conn(maps:get(config, D)).

new_ssl_conn(Config) ->
    C = new_ssl_conn(maps:get(SSLConnectConfig, Config)),
    #{SSLConn => C, {vbeam, type} => 'SSLConn'}.
