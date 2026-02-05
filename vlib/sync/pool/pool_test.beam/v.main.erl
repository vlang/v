-module('v.main').
-export([worker_s/3, worker_i/3, test_work_on_strings/0, test_work_on_ints/0]).

worker_s(P, Idx, Worker_id) ->
    Item = 'PoolProcessor.get_item'(P, Idx),
    vbeam_io:println(<<"worker_s worker_id: ", (integer_to_binary(Worker_id))/binary, " | idx: ", (integer_to_binary(Idx))/binary, " | item: ", (Item)/binary>>),
    sleep(3 * todo),
    &#{s => <<(Item)/binary, " ", (Item)/binary>>, {vbeam, type} => 'SResult'}.

worker_i(P, Idx, Worker_id) ->
    Item = 'PoolProcessor.get_item'(P, Idx),
    vbeam_io:println(<<"worker_i worker_id: ", (integer_to_binary(Worker_id))/binary, " | idx: ", (integer_to_binary(Idx))/binary, " | item: ", (integer_to_binary(Item))/binary>>),
    sleep(5 * todo),
    &#{i => Item * 1000, {vbeam, type} => 'IResult'}.

test_work_on_strings() ->
    Pool_s = new_pool_processor(#{callback => Main.worker_s, maxjobs => 8, {vbeam, type} => 'PoolProcessorConfig'}),
    'PoolProcessor.work_on_items'(Pool_s, [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>, <<"g">>, <<"h">>, <<"i">>, <<"j">>]),
    lists:foreach(fun(X) ->
        vbeam_io:println(maps:get(s, X)),
        % TODO: assert x.s.len > 1
        ok
    end, 'PoolProcessor.get_results'(Pool_s)),
    io:format("~s~n", [<<"---------- pool_s.get_results_ref: --------------">>]),
    lists:foreach(fun(X) ->
        vbeam_io:println(maps:get(s, X)),
        ok.
        % TODO: assert x.s.len > 1
        ok
    end, 'PoolProcessor.get_results_ref'(Pool_s)),

test_work_on_ints() ->
    Pool_i = new_pool_processor(#{callback => Main.worker_i, {vbeam, type} => 'PoolProcessorConfig'}),
    'PoolProcessor.work_on_items'(Pool_i, [1, 2, 3, 4, 5, 6, 7, 8]),
    lists:foreach(fun(X) ->
        vbeam_io:println(integer_to_binary(maps:get(i, X))),
        % TODO: assert x.i > 100
        ok
    end, 'PoolProcessor.get_results'(Pool_i)),
    io:format("~s~n", [<<"---------- pool_i.get_results_ref: --------------">>]),
    lists:foreach(fun(X) ->
        vbeam_io:println(integer_to_binary(maps:get(i, X))),
        ok.
        % TODO: assert x.i > 100
        ok
    end, 'PoolProcessor.get_results_ref'(Pool_i)),
