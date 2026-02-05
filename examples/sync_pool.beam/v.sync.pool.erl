-module('v.sync.pool').
-export([empty_cb/3, new_pool_processor/1, 'PoolProcessor.set_max_jobs'/2, 'PoolProcessor.work_on_items'/2, 'PoolProcessor.work_on_pointers'/2, process_in_thread/2, atomic_fetch_add_ntask/1, 'PoolProcessor.get_item'/2, 'PoolProcessor.get_result'/2, 'PoolProcessor.get_results'/1, 'PoolProcessor.get_results_ref'/1, 'PoolProcessor.set_shared_context'/2, 'PoolProcessor.get_shared_context'/1, 'PoolProcessor.set_thread_context'/3, 'PoolProcessor.get_thread_context'/2]).
% TODO: const no_result = unsafe { nil };
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

empty_cb(_p, _idx, _task_id) ->
    % TODO: {return nil;}

new_pool_processor(Context) ->
    case maps:get(callback, Context) == todo of
        true -> panic(<<"You need to pass a valid callback to new_pool_processor.">>);
        false -> ok
    end,
    Pool = #{items => [], results => [], shared_context => todo, thread_contexts => [], njobs => maps:get(maxjobs, Context), ntask => 0, thread_cb => todo, {vbeam, type} => 'PoolProcessor'},
    'WaitGroup.init'(maps:get(waitgroup, Pool)),
    &Pool.

'PoolProcessor.set_max_jobs'(Pool, Njobs) ->

'PoolProcessor.work_on_items'(Pool, Items) ->
    'PoolProcessor.work_on_pointers'(Pool, todo),
    ok.

'PoolProcessor.work_on_pointers'(Pool, Items) ->
    Njobs = nr_jobs(),
    case maps:get(njobs, Pool) > 0 of
        true -> ok;
        false -> ok
    end,
    case Njobs > length(Items) of
        true -> ok;
        false -> ok
    end,
    case Njobs < 1 of
        true -> ok;
        false -> ok
    end,
    % TODO: {pool.thread_contexts = []voidptr{len: items.len};ast.LockExpr;pool.items = []voidptr{cap: items.len};pool.items << items;pool.ntask = 0;pool.waitgroup.add(njobs);[unhandled stmt str type: v.ast.ForCStmt ];}
    'WaitGroup.wait'(maps:get(waitgroup, Pool)),
    ok.

process_in_thread(Pool, Task_id) ->
    Cb = todo,
    Ilen = length(maps:get(items, Pool)),
    % TODO: for {
    'WaitGroup.done'(maps:get(waitgroup, Pool)),
    ok.

atomic_fetch_add_ntask(Pool) ->
    Old = maps:get(ntask, Pool),
    todo,
    Old.

'PoolProcessor.get_item'(Pool, Idx) ->
    todo.

'PoolProcessor.get_result'(Pool, Idx) ->
    todo,
    ok.

'PoolProcessor.get_results'(Pool) ->
    Res = [],
    lists:foreach(fun(I) ->
        todo,
        ok
    end, lists:seq(0, length(maps:get(results, Pool)) - 1)),
    Res.

'PoolProcessor.get_results_ref'(Pool) ->
    Res = [],
    lists:foreach(fun(I) ->
        todo,
        ok
    end, lists:seq(0, length(maps:get(results, Pool)) - 1)),
    Res.

'PoolProcessor.set_shared_context'(Pool, Context) ->

'PoolProcessor.get_shared_context'(Pool) ->
    maps:get(shared_context, Pool).

'PoolProcessor.set_thread_context'(Pool, Idx, Context) ->

'PoolProcessor.get_thread_context'(Pool, Idx) ->
    lists:nth(Idx + 1, maps:get(thread_contexts, Pool)).
