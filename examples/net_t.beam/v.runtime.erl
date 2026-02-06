-module('v.runtime').
-export([nr_cpus/0, total_memory/0, free_memory/0, nr_jobs/0, is_32bit/0, is_64bit/0, is_little_endian/0, is_big_endian/0]).

nr_cpus() ->
    4.

total_memory() ->
    0.

free_memory() ->
    error(<<"free_memory: not directly available on BEAM">>).

nr_jobs() ->
    % TODO: unhandled stmt type
    ok    Cpus = nr_cpus() - 1,
    Vjobs = 'string.int'(getenv(<<"VJOBS">>)),
    case Vjobs > 0 of
        true -> ok;
        false -> ok
    end,
    case Cpus == 0 of
        true -> 1;
        false -> Cpus
        end.

is_32bit() ->
    % TODO: unhandled stmt type
    ok    false.

is_64bit() ->
    case X64 of
        true -> true;
        false -> false
        end.

is_little_endian() ->
    case Little_endian of
        true -> true;
        false -> false
        end.

is_big_endian() ->
    % TODO: unhandled stmt type
    ok    false.
