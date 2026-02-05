-module('v.main').
-export([worker_sleep/3, logger/0, main/0]).
% TODO: const args = arguments();
% TODO: const nitems = args[1].int();
% TODO: const njobs = args[2].int();
% TODO: const delay = args[3].int();
% TODO: __global msgs = ast.ChanInit;

worker_sleep(P, Item_idx, Worker_id) ->
    Item = 'PoolProcessor.get_item'(P, Item_idx),
    Msgs <- <<"# worker_id: ", (integer_to_binary(Worker_id))/binary, ", item_idx: ", (integer_to_binary(Item_idx + 1))/binary, ", item: ", (integer_to_binary(Item))/binary, ", started">>,
    sleep('string.int'(lists:nth(4, arguments())) * todo),
    Msgs <- <<"# worker_id: ", (integer_to_binary(Worker_id))/binary, ", item_idx: ", (integer_to_binary(Item_idx + 1))/binary, ", item: ", (integer_to_binary(Item))/binary, ", finished.">>,
    todo.

logger() ->
    % TODO: for {

main() ->
    T = todo,
    Msgs <- <<">>> nitems: ", (integer_to_binary('string.int'(lists:nth(2, arguments()))))/binary, " | njobs: ", (integer_to_binary('string.int'(lists:nth(3, arguments()))))/binary, " | delay_ms: ", (integer_to_binary('string.int'(lists:nth(4, arguments()))))/binary>>,
    Items = [],
    Fetcher_pool = new_pool_processor(#{callback => Main.worker_sleep, {vbeam, type} => 'PoolProcessorConfig'}),
    'PoolProcessor.work_on_items'(Fetcher_pool, Items),
    Msgs <- <<">>> done">>,
    'thread.wait'(T),
    ok.
