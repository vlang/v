-module('v.rand').
-export(['PRNG.bytes'/2, 'PRNG.read'/2, 'PRNG.i32n'/2, 'PRNG.u32n'/2, 'PRNG.u64n'/2, 'PRNG.u32_in_range'/3, 'PRNG.u64_in_range'/3, 'PRNG.i8'/1, 'PRNG.i16'/1, 'PRNG.i32'/1, 'PRNG.int'/1, 'PRNG.i64'/1, 'PRNG.int31'/1, 'PRNG.int63'/1, 'PRNG.intn'/2, 'PRNG.i64n'/2, 'PRNG.int_in_range'/3, 'PRNG.i32_in_range'/3, 'PRNG.i64_in_range'/3, 'PRNG.f32'/1, 'PRNG.f32cp'/1, 'PRNG.f64'/1, 'PRNG.f64cp'/1, 'PRNG.f32n'/2, 'PRNG.f64n'/2, 'PRNG.f32_in_range'/3, 'PRNG.f64_in_range'/3, 'PRNG.ulid'/1, 'PRNG.ulid_at_millisecond'/2, 'PRNG.string_from_set'/3, 'PRNG.string'/2, 'PRNG.hex'/2, 'PRNG.ascii'/2, 'PRNG.fill_buffer_from_set'/3, 'PRNG.bernoulli'/2, 'PRNG.normal'/2, 'PRNG.normal_pair'/2, 'PRNG.binomial'/3, 'PRNG.exponential'/2, 'PRNG.shuffle'/3, 'PRNG.shuffle_clone'/3, 'PRNG.choose'/3, 'PRNG.element'/2, 'PRNG.sample'/3, new_default/1, get_current_rng/0, set_rng/1, seed/1, u8/0, u16/0, u32/0, u64/0, u32n/1, u64n/1, u32_in_range/2, u64_in_range/2, i8/0, i16/0, i32/0, int/0, i32n/1, intn/1, int_in_range/2, i32_in_range/2, int31/0, i64/0, i64n/1, i64_in_range/2, int63/0, f32/0, f32cp/0, f64/0, f64cp/0, f32n/1, f64n/1, f32_in_range/2, f64_in_range/2, bytes/1, read/1, ulid/0, ulid_at_millisecond/1, string_from_set/2, fill_buffer_from_set/2, string/1, hex/1, ascii/1, shuffle/2, shuffle_clone/2, choose/2, element/1, sample/2, bernoulli/1, normal/1, normal_pair/1, binomial/2, exponential/1]).
% TODO: [unhandled stmt str type: v.ast.InterfaceDecl ]
% TODO: const reciprocal_2_23rd = f64(1.0) / f64(u32(1) << 23);
% TODO: const reciprocal_2_52nd = f64(1.0) / f64(u64(1) << 52);
% TODO: const ieee754_mantissa_f32_mask = (u32(1) << 23) - 1;
% TODO: const ieee754_mantissa_f64_mask = (u64(1) << 52) - 1;
% TODO: __global default_rng &rand.PRNG;
% TODO: const english_letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
% TODO: const hex_chars = '0123456789abcdef';
% TODO: const ascii_chars = '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{|}~';

'PRNG.bytes'(Rng, Bytes_needed) ->
    case Bytes_needed < 0 of
        true -> error(<<"can not read < 0 random bytes">>);
        false -> ok
    end,
    Buffer = [],
    read_internal(Rng, Buffer),
    Buffer.

'PRNG.read'(Rng, Buf) ->
    read_internal(Rng, Buf),
    ok.

'PRNG.i32n'(Rng, Max) ->
    todo.

'PRNG.u32n'(Rng, Max) ->
    case Max == 0 of
        true -> error(<<"max must be positive integer">>);
        false -> ok
    end,
    Bit_len = len_32(Max),
    case todo of
        true -> ok;
        false -> begin
            Mask = case todo of
                true -> todo;
                false -> (todo << (Bit_len + 1)) - 1
            end,
            % TODO: for {
        end
    end,
    todo.

'PRNG.u64n'(Rng, Max) ->
    case Max == 0 of
        true -> error(<<"max must be positive integer">>);
        false -> ok
    end,
    Bit_len = len_64(Max),
    case todo of
        true -> ok;
        false -> begin
            Mask = case todo of
                true -> todo;
                false -> (todo << (Bit_len + 1)) - 1
            end,
            % TODO: for {
        end
    end,
    todo.

'PRNG.u32_in_range'(Rng, Min, Max) ->
    case Max <= Min of
        true -> error(<<"max must be greater than min">>);
        false -> ok
    end,
    Min + 'PRNG.u32n'(Rng, Max - Min).

'PRNG.u64_in_range'(Rng, Min, Max) ->
    case Max <= Min of
        true -> error(<<"max must be greater than min">>);
        false -> ok
    end,
    Min + 'PRNG.u64n'(Rng, Max - Min).

'PRNG.i8'(Rng) ->
    todo.

'PRNG.i16'(Rng) ->
    todo.

'PRNG.i32'(Rng) ->
    todo.

'PRNG.int'(Rng) ->
    todo.

'PRNG.i64'(Rng) ->
    todo.

'PRNG.int31'(Rng) ->
    todo.

'PRNG.int63'(Rng) ->
    todo.

'PRNG.intn'(Rng, Max) ->
    case Max <= 0 of
        true -> error(<<"max has to be positive.">>);
        false -> ok
    end,
    todo.

'PRNG.i64n'(Rng, Max) ->
    case Max <= 0 of
        true -> error(<<"max has to be positive.">>);
        false -> ok
    end,
    todo.

'PRNG.int_in_range'(Rng, Min, Max) ->
    case Max <= Min of
        true -> error(<<"max must be greater than min">>);
        false -> ok
    end,
    Min + 'PRNG.intn'(Rng, Max - Min).

'PRNG.i32_in_range'(Rng, Min, Max) ->
    case Max <= Min of
        true -> error(<<"max must be greater than min">>);
        false -> ok
    end,
    Min + todo.

'PRNG.i64_in_range'(Rng, Min, Max) ->
    case Max <= Min of
        true -> error(<<"max must be greater than min">>);
        false -> ok
    end,
    Min + 'PRNG.i64n'(Rng, Max - Min).

'PRNG.f32'(Rng) ->
    todo.

'PRNG.f32cp'(Rng) ->
    X = 'PRNG.u32'(Rng),
    Exp = todo,
    Mask = todo << 31,
    case todo of
        true -> begin
            X1 = 'PRNG.u32'(Rng),
            Exp1 = 31,
        end;
        false -> ok
    end,
    % TODO: for {
    case Exp1 < 118 of
        true -> ok;
        false -> ok
    end,
    X2 = (Exp1 << 23) | (X1 >> 8) & (todo << 23) - 1,
    f32_from_bits(X2).

'PRNG.f64'(Rng) ->
    todo.

'PRNG.f64cp'(Rng) ->
    X = 'PRNG.u64'(Rng),
    Exp = todo,
    Mask = todo << 63,
    Bitcount = todo,
    case todo of
        true -> begin
            X1 = 'PRNG.u64'(Rng),
            Exp1 = 31,
        end;
        false -> ok
    end,
    % TODO: for {
    Exp2 = Bitcount,
    case Bitcount > 11 of
        true -> ok;
        false -> ok
    end,
    X2 = (Exp2 << 52) | (X1 & (todo << 52) - 1),
    f64_from_bits(X2).

'PRNG.f32n'(Rng, Max) ->
    case Max < 0 of
        true -> error(<<"max has to be non-negative.">>);
        false -> ok
    end,
    'PRNG.f32'(Rng) * Max.

'PRNG.f64n'(Rng, Max) ->
    case Max < 0 of
        true -> error(<<"max has to be non-negative.">>);
        false -> ok
    end,
    'PRNG.f64'(Rng) * Max.

'PRNG.f32_in_range'(Rng, Min, Max) ->
    case Max < Min of
        true -> error(<<"max must be greater than or equal to min">>);
        false -> ok
    end,
    Min + 'PRNG.f32n'(Rng, Max - Min).

'PRNG.f64_in_range'(Rng, Min, Max) ->
    case Max < Min of
        true -> error(<<"max must be greater than or equal to min">>);
        false -> ok
    end,
    Min + 'PRNG.f64n'(Rng, Max - Min).

'PRNG.ulid'(Rng) ->
    internal_ulid_at_millisecond(Rng, todo).

'PRNG.ulid_at_millisecond'(Rng, Unix_time_milli) ->
    internal_ulid_at_millisecond(Rng, Unix_time_milli).

'PRNG.string_from_set'(Rng, Charset, Len) ->
    internal_string_from_set(Rng, Charset, Len).

'PRNG.string'(Rng, Len) ->
    internal_string_from_set(Rng, <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>, Len).

'PRNG.hex'(Rng, Len) ->
    internal_string_from_set(Rng, <<"0123456789abcdef">>, Len).

'PRNG.ascii'(Rng, Len) ->
    internal_string_from_set(Rng, <<"!\"#$%&\\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\\\^_`abcdefghijklmnopqrstuvwxyz{|}~">>, Len).

'PRNG.fill_buffer_from_set'(Rng, Charset, Buf) ->
    internal_fill_buffer_from_set(Rng, Charset, Buf),
    ok.

'PRNG.bernoulli'(Rng, P) ->
    case P < 0 || P > 1 of
        true -> error(<<(float_to_binary(P))/binary, " is not a valid probability value.">>);
        false -> ok
    end,
    'PRNG.f64'(Rng) <= P.

'PRNG.normal'(Rng, Conf) ->
    X = element(1, 'PRNG.normal_pair'(Rng, Conf)),
    X.

'PRNG.normal_pair'(Rng, Conf) ->
    case maps:get(sigma, Conf) <= 0 of
        true -> error(<<"Standard deviation must be positive">>);
        false -> ok
    end,
    % TODO: for {
    error(<<"Implementation error. Please file an issue.">>).

'PRNG.binomial'(Rng, N, P) ->
    case P < 0 || P > 1 of
        true -> error(<<(float_to_binary(P))/binary, " is not a valid probability value.">>);
        false -> ok
    end,
    Count = 0,
    lists:foreach(fun(_) ->
        case 'PRNG.bernoulli'(Rng, P) of
            true -> todo;
            false -> ok
        end,
        ok
    end, lists:seq(0, N - 1)),
    Count.

'PRNG.exponential'(Rng, Lambda) ->
    case Lambda <= 0 of
        true -> panic(<<"The rate (lambda) must be positive.">>);
        false -> ok
    end,
    -mlog('PRNG.f64'(Rng)) / Lambda.

'PRNG.shuffle'(Rng, A, Config_) ->
    'unknown.validate_for'(Config_, A),
    New_end = case maps:get(end, Config_) == 0 of
        true -> length(A);
        false -> maps:get(end, Config_)
    end,
    {X, A_i} = lists:foldl(fun(I, {XAcc, A_iAcc}) ->
        XOut = 'unknown.int_in_range'(Rng, I, New_end),
        A_iOut = lists:nth(I + 1, A),
        {XOut, A_iOut}
    end, {X, A_i}, lists:seq(maps:get(start, Config_), New_end - 1 - 1)),

'PRNG.shuffle_clone'(Rng, A, Config_) ->
    Res = 'unknown.clone'(A),
    'unknown.shuffle'(Rng, Res, Config_),
    Res.

'PRNG.choose'(Rng, Array, K) ->
    N = length(Array),
    case K > N of
        true -> error(<<"Cannot choose ", " elements without replacement from a ", "-element array.">>);
        false -> ok
    end,
    Results = [],
    Indices = [],
    'unknown.shuffle'(Rng, Indices),
    lists:foreach(fun(I) ->
        ok
    end, lists:seq(0, K - 1)),
    Results.

'PRNG.element'(Rng, Array) ->
    case length(Array) == 0 of
        true -> error(<<"Cannot choose an element from an empty array.">>);
        false -> ok
    end,
    lists:nth('unknown.intn'(Rng, length(Array)) + 1, Array).

'PRNG.sample'(Rng, Array, K) ->
    Results = [],
    lists:foreach(fun(I) ->
        ok
    end, lists:seq(0, K - 1)),
    Results.

new_default(Config_) ->
    Rng = &#{{vbeam, type} => 'WyRandRNG'},
    'WyRandRNG.seed'(Rng, maps:get(seed_, Config_)),
    todo,
    todo.

get_current_rng() ->
    Default_rng.

set_rng(Rng) ->
    Default_rng = todo,

seed(Seed) ->
    'PRNG.seed'(Default_rng, Seed),
    ok.

u8() ->
    'PRNG.u8'(Default_rng).

u16() ->
    'PRNG.u16'(Default_rng).

u32() ->
    'PRNG.u32'(Default_rng).

u64() ->
    'PRNG.u64'(Default_rng).

u32n(Max) ->
    'PRNG.u32n'(Default_rng, Max).

u64n(Max) ->
    'PRNG.u64n'(Default_rng, Max).

u32_in_range(Min, Max) ->
    'PRNG.u32_in_range'(Default_rng, Min, Max).

u64_in_range(Min, Max) ->
    'PRNG.u64_in_range'(Default_rng, Min, Max).

i8() ->
    'PRNG.i8'(Default_rng).

i16() ->
    'PRNG.i16'(Default_rng).

i32() ->
    'PRNG.i32'(Default_rng).

int() ->
    'PRNG.int'(Default_rng).

i32n(Max) ->
    'PRNG.i32n'(Default_rng, Max).

intn(Max) ->
    'PRNG.intn'(Default_rng, Max).

int_in_range(Min, Max) ->
    'PRNG.int_in_range'(Default_rng, Min, Max).

i32_in_range(Min, Max) ->
    'PRNG.i32_in_range'(Default_rng, Min, Max).

int31() ->
    'PRNG.int31'(Default_rng).

i64() ->
    'PRNG.i64'(Default_rng).

i64n(Max) ->
    'PRNG.i64n'(Default_rng, Max).

i64_in_range(Min, Max) ->
    'PRNG.i64_in_range'(Default_rng, Min, Max).

int63() ->
    'PRNG.int63'(Default_rng).

f32() ->
    'PRNG.f32'(Default_rng).

f32cp() ->
    'PRNG.f32cp'(Default_rng).

f64() ->
    'PRNG.f64'(Default_rng).

f64cp() ->
    'PRNG.f64cp'(Default_rng).

f32n(Max) ->
    'PRNG.f32n'(Default_rng, Max).

f64n(Max) ->
    'PRNG.f64n'(Default_rng, Max).

f32_in_range(Min, Max) ->
    'PRNG.f32_in_range'(Default_rng, Min, Max).

f64_in_range(Min, Max) ->
    'PRNG.f64_in_range'(Default_rng, Min, Max).

bytes(Bytes_needed) ->
    'PRNG.bytes'(Default_rng, Bytes_needed).

read(Buf) ->
    read_internal(Default_rng, Buf),
    ok.

ulid() ->
    'PRNG.ulid'(Default_rng).

ulid_at_millisecond(Unix_time_milli) ->
    'PRNG.ulid_at_millisecond'(Default_rng, Unix_time_milli).

string_from_set(Charset, Len) ->
    'PRNG.string_from_set'(Default_rng, Charset, Len).

fill_buffer_from_set(Charset, Buf) ->
    'PRNG.fill_buffer_from_set'(Default_rng, Charset, Buf),
    ok.

string(Len) ->
    string_from_set(<<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>, Len).

hex(Len) ->
    string_from_set(<<"0123456789abcdef">>, Len).

ascii(Len) ->
    string_from_set(<<"!\"#$%&\\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\\\^_`abcdefghijklmnopqrstuvwxyz{|}~">>, Len).

shuffle(A, Config_) ->
    'unknown.shuffle'(Default_rng, A, Config_),
    ok.

shuffle_clone(A, Config_) ->
    'unknown.shuffle_clone'(Default_rng, A, Config_).

choose(Array, K) ->
    'unknown.choose'(Default_rng, Array, K).

element(Array) ->
    'unknown.element'(Default_rng, Array).

sample(Array, K) ->
    'unknown.sample'(Default_rng, Array, K).

bernoulli(P) ->
    'PRNG.bernoulli'(Default_rng, P).

normal(Config_) ->
    'PRNG.normal'(Default_rng, Config_).

normal_pair(Config_) ->
    'PRNG.normal_pair'(Default_rng, Config_).

binomial(N, P) ->
    'PRNG.binomial'(Default_rng, N, P).

exponential(Lambda) ->
    'PRNG.exponential'(Default_rng, Lambda).
