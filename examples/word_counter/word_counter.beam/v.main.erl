-module('v.main').
-export([main/0, extract_words/1, filter_word/1]).

main() ->
    Path = <<"cinderella.txt">>,
    case length(init:get_plain_arguments()) /= 2 of
        true -> begin
            io:format("~s~n", [<<"usage: word_counter [text_file]">>]),
            vbeam_io:println(<<"using ", (Path)/binary>>)
        end;
        false -> ok
    end,
    Contents = read_file(string:trim(Path)),
    M = #{},
    lists:foreach(fun(Word) ->
        todo,
        ok
    end, extract_words(Contents)),
    Keys = maps:keys(M),
    lists:sort(Keys),
    lists:foreach(fun(Key) ->
        Val = maps:get(Key, M),
        vbeam_io:println(<<(Key)/binary, " => ", (integer_to_binary(Val))/binary>>),
        ok.
        ok
    end, Keys),
        ok.

extract_words(Contents) ->
    Splits = [],
    lists:foreach(fun(Space_split) ->
        case case binary:match(Space_split, <<"\\n">>) of nomatch -> false; _ -> true end of
            true -> Splits bsl binary:split(Space_split, <<"\\n">>, [global]);
            false -> Splits bsl Space_split
        end,
        ok
    end, binary:split(string:lowercase(Contents), <<" ">>, [global])),
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
            Start = I,
            % TODO: unhandled stmt type
            End = I,
            lists:nth(todo + 1, Word)
        end
        end.
