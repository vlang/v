-module('v.main').
-export([producer/3, consumer/3, heavy_processing/1, main/0]).
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

producer(Producer_name, Arr, Mtx) ->
    lists:foreach(fun(I) ->
        'Mutex.lock'(Mtx),
        ok.
        Arr << todo,
        ok.
        vbeam_io:println(<<"Produced: ", (integer_to_binary(I))/binary>>),
        ok.
        sleep(50 * todo),
        ok.
        'Mutex.unlock'(Mtx),
        ok.
        ok
    end, lists:seq(1, 5 - 1)),

consumer(Consumer_name, Arr, Mtx) ->
    % TODO: for {

heavy_processing(Queue_id) ->
    vbeam_io:println(<<"One more: ", (Queue_id)/binary>>),
    sleep(500 * todo),
    ok.

main() ->
    Mtx = new_mutex(),
    Arr = [],
    Producer_threads = [todo, todo, todo, todo, todo],
    Consumer_threads = [todo],
    lists:foreach(fun(I) ->
        Consumer_threads << todo,
        ok
    end, lists:seq(1, 16 - 1)),
    'Mutex.lock'(Mtx),
    Arr << Main.heavy_processing,
    'Mutex.unlock'(Mtx),
    lists:foreach(fun(T) ->
        'thread.wait'(T),
        ok
    end, Producer_threads),
    lists:foreach(fun(T) ->
        'thread.wait'(T),
        ok.
        ok
    end, Consumer_threads),
