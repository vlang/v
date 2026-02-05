-module('v.runtime').
-export([nr_jobs/0, is_32bit/0, is_64bit/0, is_little_endian/0, is_big_endian/0]).

nr_jobs() ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    Cpus = nr_cpus() - 1,
    Vjobs = 'string.int'(getenv(<<"VJOBS">>)),
    case Vjobs > 0 of
        true -> ok;
        false -> ok
    end,
    case Cpus == 0 of
        true -> 1;
        false -> ok
    end,
    Cpus.

is_32bit() ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    false.

is_64bit() ->
    ,
    false.

is_little_endian() ->
    ,
    false.

is_big_endian() ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    false.
