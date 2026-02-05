-module('v.rand.wyrand').
-export(['WyRandRNG.seed'/2, 'WyRandRNG.u8'/1, 'WyRandRNG.u16'/1, 'WyRandRNG.u32'/1, 'WyRandRNG.block_size'/1]).
% TODO: const wyp0 = u64(0x2d358dccaa6c78a5);
% TODO: const wyp1 = u64(0x8bb84b93962eacc9);
% TODO: const seed_len = 2;

'WyRandRNG.seed'(Rng, Seed_data) ->
    case length(Seed_data) != 2 of
        true -> begin
            eprintln(<<"WyRandRNG needs 2 32-bit unsigned integers as the seed.">>),
            exit(1)
        end;
        false -> ok
    end,

'WyRandRNG.u8'(Rng) ->
    case maps:get(bytes_left, Rng) >= 1 of
        true -> begin
            Value = todo,
            Value
        end;
        false -> ok
    end,
    Value1 = todo,
    Value1.

'WyRandRNG.u16'(Rng) ->
    case maps:get(bytes_left, Rng) >= 2 of
        true -> begin
            Value = todo,
            Value
        end;
        false -> ok
    end,
    Ans = 'WyRandRNG.u64'(Rng),
    todo.

'WyRandRNG.u32'(Rng) ->
    case maps:get(bytes_left, Rng) >= 4 of
        true -> begin
            Value = todo,
            Value
        end;
        false -> ok
    end,
    Ans = 'WyRandRNG.u64'(Rng),
    todo.

'WyRandRNG.block_size'(Rng) ->
    64.
