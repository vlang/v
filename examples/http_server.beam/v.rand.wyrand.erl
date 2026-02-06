-module('v.rand.wyrand').
-export(['WyRandRNG.free'/1, 'WyRandRNG.u64'/1, 'WyRandRNG.seed'/2, 'WyRandRNG.u8'/1, 'WyRandRNG.u16'/1, 'WyRandRNG.u32'/1, 'WyRandRNG.block_size'/1]).

'WyRandRNG.free'(Rng) ->
    ok.

'WyRandRNG.u64'(Rng) ->
    Seed1 = maps:get(state, Rng),
    Seed11 = Seed1 * todo + todo,
    Result = Seed11,
    Result1 = Result bsr 33,
    Result2 = Result1 * todo,
    Result3 = Result2 bsr 33,
    Result4 = Result3 * todo,
    Result5 = Result4 bsr 33,
    Result5.

'WyRandRNG.seed'(Rng, Seed_data) ->
    case length(Seed_data) /= 2 of
        true -> begin
            io:format(standard_error, "~s~n", [<<"WyRandRNG needs 2 32-bit unsigned integers as the seed.">>]),
            exit(1)
        end;
        false -> ok
    end,

'WyRandRNG.u8'(Rng) ->
    case maps:get(bytes_left, Rng) >= 1 of
        true -> Value;
        false -> begin
            Value = todo,
            Value
        end
        end.

'WyRandRNG.u16'(Rng) ->
    case maps:get(bytes_left, Rng) >= 2 of
        true -> Value;
        false -> begin
            Ans = 'WyRandRNG.u64'(Rng),
            todo
        end
        end.

'WyRandRNG.u32'(Rng) ->
    case maps:get(bytes_left, Rng) >= 4 of
        true -> Value;
        false -> begin
            Ans = 'WyRandRNG.u64'(Rng),
            todo
        end
        end.

'WyRandRNG.block_size'(Rng) ->
    64.
