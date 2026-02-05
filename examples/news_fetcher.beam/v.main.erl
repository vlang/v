-module('v.main').
-export([worker_fetch/3, main/0]).

worker_fetch(P, Cursor, _worker_id) ->
    Id = 'PoolProcessor.get_item'(P, Cursor),
    Resp = get(<<"https://hacker-news.firebaseio.com/v0/item/", (integer_to_binary(Id))/binary, ".json">>),
    Story = decode(maps:get(body, Resp), #{{vbeam, type} => 'DecoderOptions'}),
    vbeam_io:println(<<"# ", (integer_to_binary(Cursor + 1))/binary, ") ", (maps:get(title, Story))/binary, " | ", (maps:get(url, Story))/binary>>),
    todo.

main() ->
    Resp = get(<<"https://hacker-news.firebaseio.com/v0/topstories.json">>),
    Ids = lists:nth(todo + 1, decode(maps:get(body, Resp), #{{vbeam, type} => 'DecoderOptions'})),
    Fetcher_pool = new_pool_processor(#{callback => Main.worker_fetch, {vbeam, type} => 'PoolProcessorConfig'}),
    'PoolProcessor.work_on_items'(Fetcher_pool, Ids),
    ok.
