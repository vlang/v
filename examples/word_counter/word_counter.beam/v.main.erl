-module('v.main').
-export([main/0, extract_words/1, filter_word/1]).

main() ->
    Path = <<"cinderella.txt">>,
    case length('v.os':'arguments'()) /= 2 of
        true -> begin
            io:format("~s~n", [<<"usage: word_counter [text_file]">>]),
            vbeam_io:println(<<"using ", (Path)/binary>>)
        end;
        false -> ok
    end,
    Contents = read_file('string.trim_space'(Path)),
    M = #{},
    lists:foreach(fun(Word) ->
        todo,
        ok
    end, extract_words(Contents)),
    Keys = 'map[string]int.keys'(M),
    '[]string.sort'(Keys),
    lists:foreach(fun(Key) ->
        Val = maps:get(Key, M),
        vbeam_io:println(<<(Key)/binary, " => ", (integer_to_binary(Val))/binary>>),
        ok.
        ok
    end, Keys),

extract_words(Contents) ->
    Splits = [],
    lists:foreach(fun(Space_split) ->
        case 'string.contains'(Space_split, <<"\\n">>) of
            true -> Splits bsl 'string.split'(Space_split, <<"\\n">>);
            false -> Splits bsl Space_split
        end,
        ok
    end, 'string.split'('string.to_lower'(Contents), <<" ">>)),
    Results = [],
    lists:foreach(fun(S) ->
        Result = filter_word(S),
        case Result == <<"">> of
            true -> ok;
            false -> ok
        end,
        Results bsl Result,
        ok
    end, Splits),
    Results.

filter_word(Word) ->
    case Word == <<"">> orelse Word == <<" ">> of
        true -> <<"">>;
        false -> begin
            I = 0,
            % TODO: unhandled stmt type
            ok            Start = I,
            % TODO: unhandled stmt type
            ok            End = I,
            lists:nth(todo + 1, Word)
        end
        end.
