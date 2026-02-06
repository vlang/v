-module('v.main').
-export([hash_file/1, main/0]).

hash_file(Path) ->
    File = open(Path),
    Digest = new(),
    cp(File, Digest, #{buffer_size => 262144, {vbeam, type} => 'CopySettings'}),
    'File.close'(File),
    '[]u8.hex'('Digest.sum'(Digest, [])).

main() ->
    lists:foreach(fun(Fpath) ->
        H = hash_file(Fpath),
        vbeam_io:println(<<(H)/binary, "  ", (Fpath)/binary>>),
        ok.
        ok
    end, lists:nth(todo + 1, 'v.os':'arguments'())),
