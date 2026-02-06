-module('v.sync').
-export([new_channel/1, new_channel_st/2, new_channel_st_noscan/2, 'Channel.close'/1, 'Channel.try_push'/2, 'Channel.try_pop'/2, 'Channel.push'/2, 'Channel.pop'/2, 'Channel.len'/1, 'Channel.is_closed'/1, channel_select/4, 'Mutex.str'/1, 'RwMutex.str'/1, new_cond/1, 'Cond.wait'/1, 'Cond.signal'/1, 'Cond.broadcast'/1, 'Cond.timed_wait'/2, new_many_times/1, 'ManyTimes.do'/2, 'ManyTimes.do_slow'/2, new_once/0, 'Once.do'/2, 'Once.do_slow'/2, 'Once.do_with_param'/3, 'Once.do_slow_with_param'/3, new_spin_lock/0, 'SpinLock.lock'/1, 'SpinLock.try_lock'/1, 'SpinLock.unlock'/1, 'SpinLock.destroy'/1, new_mutex/0, 'Mutex.init'/1, 'Mutex.lock'/1, 'Mutex.try_lock'/1, 'Mutex.unlock'/1, 'Mutex.destroy'/1, new_rwmutex/0, 'RwMutex.init'/1, 'RwMutex.rlock'/1, 'RwMutex.lock'/1, 'RwMutex.try_rlock'/1, 'RwMutex.try_wlock'/1, 'RwMutex.runlock'/1, 'RwMutex.unlock'/1, 'RwMutex.destroy'/1, new_semaphore/0, new_semaphore_init/1, 'Semaphore.init'/2, 'Semaphore.post'/1, 'Semaphore.wait'/1, 'Semaphore.try_wait'/1, 'Semaphore.timed_wait'/2, 'Semaphore.destroy'/1, new_waitgroup/0, 'WaitGroup.init'/1, 'WaitGroup.add'/2, 'WaitGroup.done'/1, 'WaitGroup.wait'/1, 'WaitGroup.go'/2, int_min/2, convert_t_to_voidptr/1, convert_voidptr_to_t/1, 'BufferElemStat__static__from'/1, 'Direction__static__from'/1, 'ChanState__static__from'/1]).

new_channel(N) ->
    St = case todo > 0 of
        true -> todo;
        false -> 1
    end,
    new_channel_st(N, St).

new_channel_st(N, St) ->
    Wsem = case N > 0 of
        true -> N;
        false -> 1
    end,
    Rsem = case N > 0 of
        true -> todo;
        false -> 1
    end,
    Rbuf = case N > 0 of
        true -> todo;
        false -> todo
    end,
    Sbuf = case N > 0 of
        true -> todo;
        false -> todo
    end,
    Ch = #{objsize => St, cap => N, write_free => N, read_avail => 0, ringbuf => Rbuf, statusbuf => Sbuf, write_sub_mtx => new_spin_lock(), read_sub_mtx => new_spin_lock(), {vbeam, type} => 'Channel'},
    'Semaphore.init'(maps:get(writesem, Ch), Wsem),
    'Semaphore.init'(maps:get(readsem, Ch), Rsem),
    'Semaphore.init'(maps:get(writesem_im, Ch), 0),
    'Semaphore.init'(maps:get(readsem_im, Ch), 0),
    Ch.

new_channel_st_noscan(N, St) ->
    new_channel_st(N, St).

'Channel.close'(Ch) ->
    case maps:get(closed, Ch) /= 0 of
        true -> ok;
        false -> begin
            'Semaphore.post'(maps:get(readsem_im, Ch)),
            'Semaphore.post'(maps:get(readsem, Ch)),
            'Semaphore.post'(maps:get(writesem, Ch)),
            'Semaphore.post'(maps:get(writesem_im, Ch)),
            ok
        end
        end.

'Channel.try_push'(Ch, Src) ->
    case maps:get(closed, Ch) /= 0 of
        true -> closed;
        false -> 
            case maps:get(cap, Ch) > 0 andalso maps:get(write_free, Ch) == 0 of
                true -> not_ready;
                false -> begin
                    case maps:get(cap, Ch) > 0 of
                        true -> begin
                            todo,
                            todo
                        end;
                        false -> ok
                    end,
                    'Semaphore.post'(maps:get(readsem, Ch)),
                    success
                end
                        end
                end.

'Channel.try_pop'(Ch, Dest) ->
    case maps:get(read_avail, Ch) == 0 of
        true -> not_ready;
        false -> begin
            case maps:get(cap, Ch) > 0 of
                true -> begin
                    todo,
                    todo
                end;
                false -> ok
            end,
            'Semaphore.post'(maps:get(writesem, Ch)),
            success
        end
        end.

'Channel.push'(Ch, Src) ->
    case maps:get(closed, Ch) /= 0 of
        true -> ok;
        false -> begin
            'Semaphore.wait'(maps:get(writesem, Ch)),
            case maps:get(closed, Ch) /= 0 of
                true -> ok;
                false -> begin
                    case maps:get(cap, Ch) > 0 of
                        true -> begin
                            todo,
                            todo
                        end;
                        false -> ok
                    end,
                    'Semaphore.post'(maps:get(readsem, Ch)),
                    ok
                end
                        end
        end
        end.

'Channel.pop'(Ch, Dest) ->
    'Semaphore.wait'(maps:get(readsem, Ch)),
    case maps:get(closed, Ch) /= 0 andalso maps:get(read_avail, Ch) == 0 of
        true -> false;
        false -> begin
            case maps:get(cap, Ch) > 0 of
                true -> begin
                    todo,
                    todo
                end;
                false -> ok
            end,
            'Semaphore.post'(maps:get(writesem, Ch)),
            true
        end
        end.

'Channel.len'(Ch) ->
    todo.

'Channel.is_closed'(Ch) ->
    maps:get(closed, Ch) /= 0.

channel_select(Channels, Dir, Dest, Timeout) ->
    lists:foreach(fun(Ch) ->
        case Dir of
            pop -> case 'Channel.try_pop'(Ch, Dest) == success of
                true -> I;
                false -> ok
            end;
            push -> case 'Channel.try_push'(Ch, Dest) == success of
                true -> I;
                false -> ok
            end
        end,
        ok
    end, Channels),
    -1.

'Mutex.str'(M) ->
    <<"Mutex(", (todo)/binary, ")">>.

'RwMutex.str'(M) ->
    <<"RwMutex(", (todo)/binary, ")">>.

new_cond(M) ->
    C = #{mutex => M, inner_mutex => #{{vbeam, type} => 'Mutex'}, waiters => [], {vbeam, type} => 'Cond'},
    'Mutex.init'(maps:get(inner_mutex, C)),
    C.

'Cond.wait'(C) ->
    Sem = new_semaphore(),
    % TODO: unhandled stmt type
    ok    'Mutex.lock'(maps:get(inner_mutex, C)),
    maps:get(waiters, C) bsl Sem,
    'Mutex.unlock'(maps:get(inner_mutex, C)),
    'Mutex.unlock'(maps:get(mutex, C)),
    'Semaphore.wait'(Sem),
    'Mutex.lock'(maps:get(inner_mutex, C)),
    % TODO: unhandled stmt type
    ok    'Mutex.unlock'(maps:get(inner_mutex, C)),
    'Mutex.lock'(maps:get(mutex, C)),
    ok.

'Cond.signal'(C) ->
    'Mutex.lock'(maps:get(inner_mutex, C)),
    % TODO: unhandled stmt type
    ok    case length(maps:get(waiters, C)) > 0 of
        true -> begin
            Waiter = lists:nth(1, maps:get(waiters, C)),
            'Semaphore.delete'(maps:get(waiters, C), 0),
            'Semaphore.post'(Waiter)
        end;
        false -> ok
    end.

'Cond.broadcast'(C) ->
    'Mutex.lock'(maps:get(inner_mutex, C)),
    % TODO: unhandled stmt type
    ok    Waiter = lists:foldl(fun(I, WaiterAcc) ->
        WaiterOut = lists:nth(I + 1, maps:get(waiters, C)),
        'Semaphore.post'(Waiter),
        WaiterOut
    end, Waiter, lists:seq(0, length(maps:get(waiters, C)) - 1)),
    'Semaphore.clear'(maps:get(waiters, C)),
    ok.

'Cond.timed_wait'(C, Timeout) ->
    Sem = new_semaphore(),
    % TODO: unhandled stmt type
    ok    'Mutex.lock'(maps:get(inner_mutex, C)),
    maps:get(waiters, C) bsl Sem,
    'Mutex.unlock'(maps:get(inner_mutex, C)),
    'Mutex.unlock'(maps:get(mutex, C)),
    Result = 'Semaphore.timed_wait'(Sem, Timeout),
    'Mutex.lock'(maps:get(inner_mutex, C)),
    % TODO: unhandled stmt type
    ok    'Mutex.unlock'(maps:get(inner_mutex, C)),
    'Mutex.lock'(maps:get(mutex, C)),
    Result.

new_many_times(Times) ->
    Many_times = #{times => Times, {vbeam, type} => 'ManyTimes'},
    'RwMutex.init'(maps:get(m, Many_times)),
    Many_times.

'ManyTimes.do'(M, F) ->
    case load_u64(maps:get(count, M)) < maps:get(times, M) of
        true -> 'ManyTimes.do_slow'(M, F);
        false -> ok
    end.

'ManyTimes.do_slow'(M, F) ->
    'RwMutex.lock'(maps:get(m, M)),
    case maps:get(count, M) < maps:get(times, M) of
        true -> begin
            store_u64(maps:get(count, M), maps:get(count, M) + 1),
            f()
        end;
        false -> ok
    end,
    'RwMutex.unlock'(maps:get(m, M)),
    ok.

new_once() ->
    Once = #{{vbeam, type} => 'Once'},
    'RwMutex.init'(maps:get(m, Once)),
    Once.

'Once.do'(O, F) ->
    case load_u64(maps:get(count, O)) < 1 of
        true -> 'Once.do_slow'(O, F);
        false -> ok
    end.

'Once.do_slow'(O, F) ->
    'RwMutex.lock'(maps:get(m, O)),
    case maps:get(count, O) < 1 of
        true -> begin
            store_u64(maps:get(count, O), 1),
            f()
        end;
        false -> ok
    end,
    'RwMutex.unlock'(maps:get(m, O)),
    ok.

'Once.do_with_param'(O, F, Param) ->
    case load_u64(maps:get(count, O)) < 1 of
        true -> 'Once.do_slow_with_param'(O, F, Param);
        false -> ok
    end.

'Once.do_slow_with_param'(O, F, Param) ->
    'RwMutex.lock'(maps:get(m, O)),
    case maps:get(count, O) < 1 of
        true -> begin
            store_u64(maps:get(count, O), 1),
            f(Param)
        end;
        false -> ok
    end,
    'RwMutex.unlock'(maps:get(m, O)),
    ok.

new_spin_lock() ->
    #{locked => 0, {vbeam, type} => 'SpinLock'}.

'SpinLock.lock'(S) ->
    Spin_count = 0,
    Max_spins = 100,
    Base_delay = 100,
    % TODO: unhandled stmt type
    ok
'SpinLock.try_lock'(S) ->
    % TODO: unhandled stmt type
    ok    false.

'SpinLock.unlock'(S) ->
    % TODO: unhandled stmt type
    ok
'SpinLock.destroy'(S) ->
    ok.

new_mutex() ->
    M = #{{vbeam, type} => 'Mutex'},
    'Mutex.init'(M),
    M.

'Mutex.init'(M) ->

'Mutex.lock'(M) ->
    % TODO: unhandled stmt type
    ok
'Mutex.try_lock'(M) ->
    case not maps:get(locked, M) of
        true -> true;
        false -> false
        end.

'Mutex.unlock'(M) ->

'Mutex.destroy'(M) ->
    ok.

new_rwmutex() ->
    M = #{{vbeam, type} => 'RwMutex'},
    'RwMutex.init'(M),
    M.

'RwMutex.init'(M) ->

'RwMutex.rlock'(M) ->
    % TODO: unhandled stmt type
    ok    todo,
    ok.

'RwMutex.lock'(M) ->
    % TODO: unhandled stmt type
    ok
'RwMutex.try_rlock'(M) ->
    case not maps:get(writer, M) of
        true -> true;
        false -> false
        end.

'RwMutex.try_wlock'(M) ->
    case not maps:get(writer, M) andalso maps:get(readers, M) == 0 of
        true -> true;
        false -> false
        end.

'RwMutex.runlock'(M) ->
    case maps:get(readers, M) > 0 of
        true -> todo;
        false -> ok
    end.

'RwMutex.unlock'(M) ->
    case maps:get(writer, M) of
        true -> ok;
        false -> case maps:get(readers, M) > 0 of
            true -> todo;
            false -> ok
        end
    end.

'RwMutex.destroy'(M) ->
    ok.

new_semaphore() ->
    new_semaphore_init(0).

new_semaphore_init(N) ->
    Sem = #{{vbeam, type} => 'Semaphore'},
    'Semaphore.init'(Sem, N),
    Sem.

'Semaphore.init'(Sem, N) ->

'Semaphore.post'(Sem) ->
    todo,
    ok.

'Semaphore.wait'(Sem) ->
    % TODO: unhandled stmt type
    ok    todo,
    ok.

'Semaphore.try_wait'(Sem) ->
    case maps:get(count, Sem) > 0 of
        true -> true;
        false -> false
        end.

'Semaphore.timed_wait'(Sem, Timeout) ->
    Start = now(),
    % TODO: unhandled stmt type
    ok    todo,
    true.

'Semaphore.destroy'(Sem) ->
    ok.

new_waitgroup() ->
    Wg = #{{vbeam, type} => 'WaitGroup'},
    'WaitGroup.init'(Wg),
    Wg.

'WaitGroup.init'(Wg) ->
    'Semaphore.init'(maps:get(sem, Wg), 0),
    ok.

'WaitGroup.add'(Wg, Delta) ->
    Old_nrjobs = todo,
    case Delta >= 0 of
        true -> ok;
        false -> case maps:get(task_count, Wg) >= todo of
            true -> ok;
            false -> panic(<<"Negative number of jobs in waitgroup">>)
        end
    end,
    New_nrjobs = Old_nrjobs + Delta,
    case New_nrjobs < 0 of
        true -> panic(<<"Negative number of jobs in waitgroup">>);
        false -> ok
    end,
    case New_nrjobs == 0 andalso maps:get(wait_count, Wg) > 0 of
        true -> begin
            Num_waiters = maps:get(wait_count, Wg),
            lists:foreach(fun(_) ->
                'Semaphore.post'(maps:get(sem, Wg)),
                ok.
                ok
            end, lists:seq(0, Num_waiters - 1)),
        end;
        false -> ok
    end.

'WaitGroup.done'(Wg) ->
    'WaitGroup.add'(Wg, -1),
    ok.

'WaitGroup.wait'(Wg) ->
    case maps:get(task_count, Wg) == 0 of
        true -> ok;
        false -> begin
            todo,
            'Semaphore.wait'(maps:get(sem, Wg)),
            ok
        end
        end.

'WaitGroup.go'(Wg, F) ->
    'WaitGroup.add'(Wg, 1),
    todo,
    ok.

int_min(A, B) ->
    case A < B of
        true -> A;
        false -> B
        end.

convert_t_to_voidptr(Value) ->
    F = #{{vbeam, type} => 'DataConversion'},
    todo.

convert_voidptr_to_t(Value) ->
    F = #{f_voidptr => Value, {vbeam, type} => 'DataConversion'},
    error(<<"Unsupported data type `", "` from voidptr">>).

'BufferElemStat__static__from'(Input) ->
    error(<<"invalid value">>).

'Direction__static__from'(Input) ->
    error(<<"invalid value">>).

'ChanState__static__from'(Input) ->
    error(<<"invalid value">>).
