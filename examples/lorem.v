/*
Random Markov Text Generator

This program generates pseudo-random text using a Markov chain built from
one of several embedded corpora in the strings module. It produces structured output in the
form of paragraphs and sentences, with configurable parameters.

Usage:

    v run lorem.v [--options]
    ./lorem [--options]

Example:

    ./lorem -order 2 -words 12 -sentences 4 -paragraphs 3 -corpus poe
*/
import strings.lorem
import flag
import os
import rand
import time

fn main() {
	mut fp := flag.new_flag_parser(os.args[1..])
	fp.application('lorem')
	fp.version('1.0')
	fp.description('Random text generator using a Markov chain')

	order := fp.int('order', `o`, 2, 'Markov order [default: 2]')
	words_per_sentence := fp.int('words', `w`, 10, 'Words per sentence [default: 10]')
	sentences_per_paragraph := fp.int('sentences', `s`, 5, 'Sentences per paragraph [default: 5]')
	paragraphs := fp.int('paragraphs', `p`, 3, 'Paragraph count [default: 3]')
	corpus_name := fp.string('corpus', `c`, 'lorem', 'Corpus name (lorem, poe, darwin, bard) [default: lorem]')
	seed_text := fp.string('seed', `S`, '', 'Seed phrase (random if omitted)')
	mut rng_seed := fp.int('rngseed', `r`, 0, 'RNG seed (0 = random)')

	fp.finalize() or {
		eprintln(err)
		return
	}

	if rng_seed == 0 {
		t := time.now().unix_milli()
		rand.seed([u32(t), u32(t >> 32)])
		rng_seed = rand.int()
	}

	text := lorem.generate(
		markov_order:            order
		words_per_sentence:      words_per_sentence
		sentences_per_paragraph: sentences_per_paragraph
		paragraphs:              paragraphs
		corpus_name:             corpus_name
		seed_text:               seed_text
		rng_seed:                rng_seed
	)

	println(text)
}
