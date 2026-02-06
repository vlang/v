-module('v.crypto.sha256').
-export(['Digest.free'/1, 'Digest.init'/1, 'Digest.reset'/1, 'Digest.clone'/1, new/0, new224/0, 'Digest.write'/2, 'Digest.sum'/2, 'Digest.checksum'/1, sum/1, sum256/1, sum224/1, block/2, 'Digest.size'/1, 'Digest.block_size'/1, hexhash/1, hexhash_224/1, block_generic/2]).

'Digest.free'(D) ->
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
        ok.

'Digest.init'(D) ->
    'Digest.reset'(D),
    ok.

'Digest.reset'(D) ->
    case not maps:get(is224, D) of
        true -> begin
        end;
        false -> begin
        end
    end,

'Digest.clone'(D) ->
    #{h => maps:get(h, D), x => maps:get(x, D), {vbeam, type} => 'Digest'}.

new() ->
    D = #{{vbeam, type} => 'Digest'},
    'Digest.init'(D),
    D.

new224() ->
    D = #{{vbeam, type} => 'Digest'},
    'Digest.init'(D),
    D.

'Digest.write'(D, P_) ->
    % TODO: unhandled stmt type
        ok.

'Digest.sum'(D, B_in) ->
    D0 = 'Digest.clone'(D),
    Hash = 'Digest.checksum'(D0),
    B_out = B_in,
    case maps:get(is224, D0) of
        true -> ok;
        false -> ok
    end,
    B_out.

'Digest.checksum'(D) ->
    Len = length(D),
    Tmp = [],
    case todo rem 64 < 56 of
        true -> 'Digest.write'(D, lists:nth(todo + 1, Tmp));
        false -> 'Digest.write'(D, lists:nth(todo + 1, Tmp))
    end,
    Len1 = todo,
    big_endian_put_u64(Tmp, Len1),
    'Digest.write'(D, lists:nth(todo + 1, Tmp)),
    case maps:get(nx, D) /= 0 of
        true -> erlang:error({panic, <<"d.nx != 0">>});
        false -> ok
    end,
    Digest = [],
    big_endian_put_u32(Digest, lists:nth(1, maps:get(h, D))),
    big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(2, maps:get(h, D))),
    big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(3, maps:get(h, D))),
    big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(4, maps:get(h, D))),
    big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(5, maps:get(h, D))),
    big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(6, maps:get(h, D))),
    big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(7, maps:get(h, D))),
    case not maps:get(is224, D) of
        true -> big_endian_put_u32(lists:nth(todo + 1, Digest), lists:nth(8, maps:get(h, D)));
        false -> ok
    end,
    Digest.

sum(Data) ->
    sum256(Data).

sum256(Data) ->
    D = new(),
    'Digest.write'(D, Data),
    'Digest.checksum'(D).

sum224(Data) ->
    D = new224(),
    'Digest.write'(D, Data),
    Sum = 'Digest.checksum'(D),
    Sum224 = [],
    copy(Sum224, lists:nth(todo + 1, Sum)),
    Sum224.

block(Dig, P) ->
    block_generic(Dig, P),
    ok.

'Digest.size'(D) ->
    case not maps:get(is224, D) of
        true -> 32;
        false -> 28
        end.

'Digest.block_size'(D) ->
    64.

hexhash(S) ->
    '[]u8.hex'(sum256(binary_to_list(S))).

hexhash_224(S) ->
    '[]u8.hex'(sum224(binary_to_list(S))).

block_generic(Dig, P_) ->
    % TODO: unhandled stmt type
        ok.
