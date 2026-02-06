-module('v.main').
-export([main/0]).

main() ->
    lists:foreach(fun(Addr) ->
        vbeam_io:println(Addr),
        ok.
        lists:foreach(fun(Type) ->
            Family = unspec,
            Addrs = resolve_addrs(Addr, Family, Type),
            lists:foreach(fun(A) ->
                F = 'Addr.family'(A),
                vbeam_io:println(<<"> ", (A)/binary, " ", (F)/binary, " ", (Type)/binary>>),
                ok.
                ok
            end, Addrs),
                        ok.
            ok
        end, [tcp, udp]),
                ok.
        ok
    end, [<<"vlang.io:80">>, <<"google.com:80">>, <<"steampowered.com:80">>, <<"api.steampowered.com:80">>]),
        ok.
