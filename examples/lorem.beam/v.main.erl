-module('v.main').
-export([main/0]).

main() ->
    Fp = new_flag_parser(lists:nth(todo + 1, init:get_plain_arguments())),
    'FlagParser.application'(Fp, <<"lorem">>),
    'FlagParser.version'(Fp, <<"1.0">>),
    'FlagParser.description'(Fp, <<"Random text generator using a Markov chain">>),
    Order = 'FlagParser.int'(Fp, <<"order">>, todo, 2, <<"Markov order [default: 2]">>, #{{vbeam, type} => 'FlagConfig'}),
    Words_per_sentence = 'FlagParser.int'(Fp, <<"words">>, todo, 10, <<"Words per sentence [default: 10]">>, #{{vbeam, type} => 'FlagConfig'}),
    Sentences_per_paragraph = 'FlagParser.int'(Fp, <<"sentences">>, todo, 5, <<"Sentences per paragraph [default: 5]">>, #{{vbeam, type} => 'FlagConfig'}),
    Paragraphs = 'FlagParser.int'(Fp, <<"paragraphs">>, todo, 3, <<"Paragraph count [default: 3]">>, #{{vbeam, type} => 'FlagConfig'}),
    Corpus_name = 'FlagParser.string'(Fp, <<"corpus">>, todo, <<"lorem">>, <<"Corpus name (lorem, poe, darwin, bard) [default: lorem]">>, #{{vbeam, type} => 'FlagConfig'}),
    Seed_text = 'FlagParser.string'(Fp, <<"seed">>, todo, <<"">>, <<"Seed phrase (random if omitted)">>, #{{vbeam, type} => 'FlagConfig'}),
    Rng_seed = 'FlagParser.int'(Fp, <<"rngseed">>, todo, 0, <<"RNG seed (0 = random)">>, #{{vbeam, type} => 'FlagConfig'}),
    'FlagParser.finalize'(Fp),
    case Rng_seed == 0 of
        true -> begin
            T = 'Time.unix_milli'(now()),
            seed([todo, todo]),
            Rng_seed1 = int(),
        end;
        false -> ok
    end,
    Text = generate(#{markov_order => Order, words_per_sentence => Words_per_sentence, sentences_per_paragraph => Sentences_per_paragraph, paragraphs => Paragraphs, corpus_name => Corpus_name, seed_text => Seed_text, rng_seed => Rng_seed1, {vbeam, type} => 'LoremCfg'}),
    vbeam_io:println(Text),
    ok.
