-module('v.encoding.utf8.validate').
-export([utf8_string/1, utf8_data/2]).

utf8_string(S) ->
    utf8_data(maps:get(str, S), length(S)).

utf8_data(Data, Len) ->
    State = 0,
    % TODO: unhandled stmt type
    ok    State == 0.
