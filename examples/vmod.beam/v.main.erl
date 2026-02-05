-module('v.main').
-export([main/0]).

main() ->
    Mod = decode(todo),
    vbeam_io:println(<<(maps:get(name, Mod))/binary, " has version ", (maps:get(version, Mod))/binary>>),
    vbeam_io:println(<<"\\nThe full mod struct: \\n", (Mod)/binary>>),
    ok.
