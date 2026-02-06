-module('v.strings.textscanner').
-export([new/1, 'TextScanner.free'/1, 'TextScanner.remaining'/1, 'TextScanner.next'/1, 'TextScanner.skip'/1, 'TextScanner.skip_n'/2, 'TextScanner.peek'/1, 'TextScanner.peek_u8'/1, 'TextScanner.peek_n'/2, 'TextScanner.peek_n_u8'/2, 'TextScanner.back'/1, 'TextScanner.back_n'/2, 'TextScanner.peek_back'/1, 'TextScanner.peek_back_n'/2, 'TextScanner.current'/1, 'TextScanner.reset'/1, 'TextScanner.goto_end'/1, 'TextScanner.skip_whitespace'/1]).

new(Input) ->
    #{input => Input, ilen => length(Input), {vbeam, type} => 'TextScanner'}.

'TextScanner.free'(Ss) ->
    todo,
    ok.

'TextScanner.remaining'(Ss) ->
    maps:get(ilen, Ss) - maps:get(pos, Ss).

'TextScanner.next'(Ss) ->
    case maps:get(pos, Ss) < maps:get(ilen, Ss) of
        true -> lists:nth(Opos + 1, maps:get(input, Ss));
        false -> -1
        end.

'TextScanner.skip'(Ss) ->
    case maps:get(pos, Ss) < maps:get(ilen, Ss) of
        true -> todo;
        false -> ok
    end.

'TextScanner.skip_n'(Ss, N) ->
    case maps:get(pos, Ss) > maps:get(ilen, Ss) of
        true -> ok;
        false -> ok
    end.

'TextScanner.peek'(Ss) ->
    case maps:get(pos, Ss) < maps:get(ilen, Ss) of
        true -> lists:nth(maps:get(pos, Ss) + 1, maps:get(input, Ss));
        false -> -1
        end.

'TextScanner.peek_u8'(Ss) ->
    case maps:get(pos, Ss) < maps:get(ilen, Ss) of
        true -> lists:nth(maps:get(pos, Ss) + 1, maps:get(input, Ss));
        false -> 0
        end.

'TextScanner.peek_n'(Ss, N) ->
    case maps:get(pos, Ss) + N < maps:get(ilen, Ss) of
        true -> lists:nth(maps:get(pos, Ss) + N + 1, maps:get(input, Ss));
        false -> -1
        end.

'TextScanner.peek_n_u8'(Ss, N) ->
    case maps:get(pos, Ss) + N < maps:get(ilen, Ss) of
        true -> lists:nth(maps:get(pos, Ss) + N + 1, maps:get(input, Ss));
        false -> 0
        end.

'TextScanner.back'(Ss) ->
    case maps:get(pos, Ss) > 0 of
        true -> todo;
        false -> ok
    end.

'TextScanner.back_n'(Ss, N) ->
    case maps:get(pos, Ss) < 0 of
        true -> ok;
        false -> ok
    end,
    case maps:get(pos, Ss) > maps:get(ilen, Ss) of
        true -> ok;
        false -> ok
    end.

'TextScanner.peek_back'(Ss) ->
    'TextScanner.peek_back_n'(Ss, 1).

'TextScanner.peek_back_n'(Ss, N) ->
    Offset = N + 1,
    case maps:get(pos, Ss) >= Offset of
        true -> lists:nth(maps:get(pos, Ss) - Offset + 1, maps:get(input, Ss));
        false -> -1
        end.

'TextScanner.current'(Ss) ->
    case maps:get(pos, Ss) > 0 of
        true -> lists:nth(maps:get(pos, Ss) - 1 + 1, maps:get(input, Ss));
        false -> -1
        end.

'TextScanner.reset'(Ss) ->

'TextScanner.goto_end'(Ss) ->

'TextScanner.skip_whitespace'(Ss) ->
    % TODO: unhandled stmt type
        ok.
