-module('v.sync.stdatomic').
-export([add_u64/2, sub_u64/2, add_i64/2, sub_i64/2, store_u64/2, load_u64/1, store_i64/2, load_i64/1, exchange_u64/2, exchange_i64/2, compare_exchange_strong_u64/3, compare_exchange_strong_i64/3, compare_exchange_weak_u64/3, compare_exchange_weak_i64/3, add_u32/2, sub_u32/2, store_u32/2, load_u32/1, exchange_u32/2, compare_exchange_strong_u32/3, new_atomic/1, 'AtomicVal.load'/1, 'AtomicVal.store'/2, 'AtomicVal.add'/2, 'AtomicVal.sub'/2, 'AtomicVal.swap'/2, 'AtomicVal.compare_and_swap'/3, thread_fence/1, cpu_relax/0, 'MemoryOrder__static__from'/1]).

add_u64(Ptr, Delta) ->
    % TODO: unhandled stmt type
    ok
sub_u64(Ptr, Delta) ->
    % TODO: unhandled stmt type
    ok
add_i64(Ptr, Delta) ->
    % TODO: unhandled stmt type
    ok
sub_i64(Ptr, Delta) ->
    % TODO: unhandled stmt type
    ok
store_u64(Ptr, Val) ->
    % TODO: unhandled stmt type
    ok
load_u64(Ptr) ->
    *Ptr.

store_i64(Ptr, Val) ->
    % TODO: unhandled stmt type
    ok
load_i64(Ptr) ->
    *Ptr.

exchange_u64(Ptr, Val) ->
    % TODO: unhandled stmt type
    ok
exchange_i64(Ptr, Val) ->
    % TODO: unhandled stmt type
    ok
compare_exchange_strong_u64(Ptr, Expected, Desired) ->
    % TODO: unhandled stmt type
    ok
compare_exchange_strong_i64(Ptr, Expected, Desired) ->
    % TODO: unhandled stmt type
    ok
compare_exchange_weak_u64(Ptr, Expected, Desired) ->
    compare_exchange_strong_u64(Ptr, Expected, Desired).

compare_exchange_weak_i64(Ptr, Expected, Desired) ->
    compare_exchange_strong_i64(Ptr, Expected, Desired).

add_u32(Ptr, Delta) ->
    % TODO: unhandled stmt type
    ok
sub_u32(Ptr, Delta) ->
    % TODO: unhandled stmt type
    ok
store_u32(Ptr, Val) ->
    % TODO: unhandled stmt type
    ok
load_u32(Ptr) ->
    *Ptr.

exchange_u32(Ptr, Val) ->
    % TODO: unhandled stmt type
    ok
compare_exchange_strong_u32(Ptr, Expected, Desired) ->
    % TODO: unhandled stmt type
    ok
new_atomic(Val) ->
    todo.

'AtomicVal.load'(A) ->
    maps:get(val, A).

'AtomicVal.store'(A, Val) ->
    % TODO: unhandled stmt type
    ok
'AtomicVal.add'(A, Delta) ->
    Old = maps:get(val, A),
    % TODO: unhandled stmt type
    ok    Old.

'AtomicVal.sub'(A, Delta) ->
    Old = maps:get(val, A),
    % TODO: unhandled stmt type
    ok    Old.

'AtomicVal.swap'(A, New) ->
    Old = maps:get(val, A),
    % TODO: unhandled stmt type
    ok    Old.

'AtomicVal.compare_and_swap'(A, Expected, New) ->
    case maps:get(val, A) == Expected of
        true -> true;
        false -> false
        end.

thread_fence(Order) ->
    ok.

cpu_relax() ->
    ok.

'MemoryOrder__static__from'(Input) ->
    error(<<"invalid value">>).
