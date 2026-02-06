-module('v.strings.lorem').
-export([generate/1, 'LorumRNG.intn'/2, lorem_vary/3, lorem_select_corpus/2, lorem_random_seed_phrase/1, lorem_tokenize/1, lorem_build_markov/2]).

generate(Cfg) ->
    Initial_seed = case maps:get(rng_seed, Cfg) /= 0 of
        true -> todo;
        false -> todo
    end,
    Rng = #{seed => Initial_seed, {vbeam, type} => 'LorumRNG'},
    Seed = case maps:get(seed_text, Cfg) /= <<"">> of
        true -> maps:get(seed_text, Cfg);
        _ -> lorem_random_seed_phrase(Rng)
    end,
    Corpus = lorem_select_corpus(Rng, maps:get(corpus_name, Cfg)),
    Tokens = lorem_tokenize(Corpus),
    case length(Tokens) =< maps:get(markov_order, Cfg) of
        true -> <<"">>;
        false -> begin
            Model = lorem_build_markov(Tokens, maps:get(markov_order, Cfg)),
            State = lorem_tokenize(Seed),
            case length(State) < maps:get(markov_order, Cfg) of
                true -> begin
                    Start = 'LorumRNG.intn'(Rng, length(Tokens) - maps:get(markov_order, Cfg)),
                    State1 = '[]string.clone'(lists:nth(todo + 1, Tokens)),
                end;
                false -> ok
            end,
            Out = [],
            Sentences = lists:foldl(fun(Pi, SentencesAcc) ->
                case Pi /= 0 of
                    true -> Out bsl <<"\\n\\n">>;
                    false -> ok
                end,
                SentencesOut = lorem_vary(Rng, maps:get(sentences_per_paragraph, Cfg), 1),
                {Words, Sentence} = lists:foldl(fun(Si, {WordsAcc, SentenceAcc}) ->
                    case Si /= 0 of
                        true -> Out bsl <<" ">>;
                        false -> ok
                    end,
                    WordsOut = lorem_vary(Rng, maps:get(words_per_sentence, Cfg), 3),
                    SentenceOut = [],
                    {Key, Nexts, Next, State2} = lists:foldl(fun(_, {KeyAcc, NextsAcc, NextAcc, StateAcc}) ->
                        KeyOut = '[]string.join'(State2, <<"">>),
                        NextsOut = maps:get(Key, Model),
                        NextOut = lists:nth('LorumRNG.intn'(Rng, length(Nexts)) + 1, Nexts),
                        Sentence bsl Next,
                        StateOut = '[]string.clone'(lists:nth(todo + 1, State2)),
                        State2 bsl Next,
                        {KeyOut, NextsOut, NextOut, StateOut}
                    end, {Key, Nexts, Next, State1}, lists:seq(0, Words - 1)),
                    case length(Sentence) > 0 of
                        true -> begin
                            Out bsl 'string.capitalize'('[]string.join'(Sentence, <<" ">>)),
                            Out bsl <<".">>
                        end;
                        false -> ok
                    end,
                    {WordsOut, SentenceOut}
                end, {Words, Sentence}, lists:seq(0, Sentences - 1)),
                SentencesOut
            end, Sentences, lists:seq(0, maps:get(paragraphs, Cfg) - 1)),
            '[]string.join'(Out, <<"">>)
        end
        end.

'LorumRNG.intn'(R, Max) ->
    case Max =< 0 of
        true -> 0;
        false -> begin
            todo
        end
        end.

lorem_vary(Rng, Base, Min) ->
    Delta = todo,
    case Delta == 0 of
        true -> Base;
        false -> begin
            Offset = 'LorumRNG.intn'(Rng, Delta * 2 + 1) - Delta,
            Val = Base + Offset,
            case Val < Min of
                true -> Min;
                false -> Val
            end
        end
        end.

lorem_select_corpus(Rng, Name) ->
    case Name /= <<"">> of
        true -> begin
            case todo of
                true -> Corpus;
                false -> ok
            end,
            eprintln(<<"unknown corpus: ", (Name)/binary>>),
            exit(1)
        end;
        false -> ok
    end,
    Keys = 'map[string]string.keys'(#{<<"lorem">> => <<"\nlorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor\nincididunt ut labore et dolore magna aliqua Ut enim ad minim veniam quis\nnostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore\neu fugiat nulla pariatur Excepteur sint occaecat cupidatat non proident sunt\nin culpa qui officia deserunt mollit anim id est laborum\n">>, <<"poe">> => <<"\nonce upon a midnight dreary while I pondered weak and weary\nover many a quaint and curious volume of forgotten lore\nwhile I nodded nearly napping suddenly there came a tapping\nas of someone gently rapping at my chamber door\n">>, <<"darwin">> => <<"\nwhen we look to the individuals of the same variety or sub variety of our\nolder cultivated plants and animals one of the first points which strikes\nus is that they generally differ much more from each other than do the\nindividuals of any one species or variety in a state of nature\n">>, <<"bard">> => <<"\nto be or not to be that is the question\nall the worlds a stage and all the men and women merely players\nthe lady doth protest too much methinks\na rose by any other name would smell as sweet\net tu brute\nif music be the food of love play on\nnow is the winter of our discontent\nwe are such stuff as dreams are made on\nbrevity is the soul of wit\nsome are born great some achieve greatness and some have greatness thrust upon them\ncry havoc and let slip the dogs of war\nall that glisters is not gold\nthe fault dear brutus is not in our stars but in ourselves\nto thine own self be true\nlord what fools these mortals be\nshall i compare thee to a summers day\n">>}),
    Key = lists:nth('LorumRNG.intn'(Rng, length(Keys)) + 1, Keys),
    maps:get(Key, #{<<"lorem">> => <<"\nlorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor\nincididunt ut labore et dolore magna aliqua Ut enim ad minim veniam quis\nnostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore\neu fugiat nulla pariatur Excepteur sint occaecat cupidatat non proident sunt\nin culpa qui officia deserunt mollit anim id est laborum\n">>, <<"poe">> => <<"\nonce upon a midnight dreary while I pondered weak and weary\nover many a quaint and curious volume of forgotten lore\nwhile I nodded nearly napping suddenly there came a tapping\nas of someone gently rapping at my chamber door\n">>, <<"darwin">> => <<"\nwhen we look to the individuals of the same variety or sub variety of our\nolder cultivated plants and animals one of the first points which strikes\nus is that they generally differ much more from each other than do the\nindividuals of any one species or variety in a state of nature\n">>, <<"bard">> => <<"\nto be or not to be that is the question\nall the worlds a stage and all the men and women merely players\nthe lady doth protest too much methinks\na rose by any other name would smell as sweet\net tu brute\nif music be the food of love play on\nnow is the winter of our discontent\nwe are such stuff as dreams are made on\nbrevity is the soul of wit\nsome are born great some achieve greatness and some have greatness thrust upon them\ncry havoc and let slip the dogs of war\nall that glisters is not gold\nthe fault dear brutus is not in our stars but in ourselves\nto thine own self be true\nlord what fools these mortals be\nshall i compare thee to a summers day\n">>}).

lorem_random_seed_phrase(Rng) ->
    lists:nth('LorumRNG.intn'(Rng, length([<<"in the beginning">>, <<"once upon a time">>, <<"it was the first">>, <<"when we consider">>, <<"there was a moment">>])) + 1, [<<"in the beginning">>, <<"once upon a time">>, <<"it was the first">>, <<"when we consider">>, <<"there was a moment">>]).

lorem_tokenize(Text) ->
    '[]string.filter'('string.split'('string.replace_each'(Text, [<<"\\n">>, <<" ">>, <<"\\t">>, <<" ">>]), <<" ">>), length(It) > 0).

lorem_build_markov(Tokens, Order) ->
    Model = #{},
    Key = lists:foldl(fun(I, KeyAcc) ->
        KeyOut = '[]string.join'(lists:nth(todo + 1, Tokens), <<"">>),
        maps:get(Key, Model) bsl lists:nth(I + Order + 1, Tokens),
        KeyOut
    end, Key, lists:seq(0, length(Tokens) - Order - 1)),
    Model.
