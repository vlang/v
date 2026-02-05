-module('v.main').
-export([diff_files/2, main/0]).

diff_files(Src_file, Dst_file) ->
    Src = read_lines(Src_file),
    Dst = read_lines(Dst_file),
    Ctx = diff(Src, Dst),
    'DiffContext[string].generate_patch'(Ctx, #{colorful => true, block_header => true, {vbeam, type} => 'DiffGenStrParam'}).

main() ->
    F1 = <<"Module{\n\tname: 'Foo'\n\tdescription: 'Awesome V module.'\n\tversion: '0.0.0'\n\tdependencies: []\n}\n">>,
    F2 = <<"Module{\n\tname: 'foo'\n\tdescription: 'Awesome V module.'\n\tversion: '0.1.0'\n\tlicense: 'MIT'\n\tdependencies: []\n}\n">>,
    P1 = <<"diff_f1.txt">>,
    P2 = <<"diff_f2.txt">>,
    write_file(P1, F1),
    write_file(P2, F2),
    Str = diff_files(P1, P2),
    vbeam_io:println(Str),
    rm(P1),
    rm(P2),
    ok.
