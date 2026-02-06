-module('v.main').
-export([worker_sleep/3, logger/0, main/0]).

worker_sleep(P, Item_idx, Worker_id) ->
    Item = 'PoolProcessor.get_item'(P, Item_idx),
    Msgs <- <<"# worker_id: ", (integer_to_binary(Worker_id))/binary, ", item_idx: ", (integer_to_binary(Item_idx + 1))/binary, ", item: ", (integer_to_binary(Item))/binary, ", started">>,
    timer:sleep(binary_to_integer(lists:nth(4, init:get_plain_arguments())) * todo),
    Msgs <- <<"# worker_id: ", (integer_to_binary(Worker_id))/binary, ", item_idx: ", (integer_to_binary(Item_idx + 1))/binary, ", item: ", (integer_to_binary(Item))/binary, ", finished.">>,
    todo.

logger() ->
    % TODO: unhandled stmt type
        ok.

main() ->
    T = todo,
    Msgs <- <<">>> nitems: ", (integer_to_binary(binary_to_integer(lists:nth(2, init:get_plain_arguments()))))/binary, " | njobs: ", (integer_to_binary(binary_to_integer(lists:nth(3, init:get_plain_arguments()))))/binary, " | delay_ms: ", (integer_to_binary(binary_to_integer(lists:nth(4, init:get_plain_arguments()))))/binary>>,
    Items = [],
    Fetcher_pool = new_pool_processor(#{callback => Main.worker_sleep, {vbeam, type} => 'PoolProcessorConfig'}),
    'PoolProcessor.work_on_items'(Fetcher_pool, Items),
    Msgs <- <<">>> done">>,
    'thread.wait'(T),
    ok.
