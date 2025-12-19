import rand
import strings
import flag
import os

// Features:
// - All parameters configurable via flags
// - Reproducible output via --seed
// - No invalid punctuation sequences
// - Em-dashes never near sentence edges

// ---------------- Configuration ----------------

pub struct LoremCfg {
	paragraphs              int = 3
	words_per_sentence      int = 10
	sentences_per_paragraph int = 5
	commas_per_sentence     int = 2
}

// ---------------- Generator --------------------

pub fn generate_lorem(cfg LoremCfg) string {
	lorem_words :=
		'lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor ' +
		'incididunt ut labore et dolore magna aliqua enim ad minim veniam quis nostrud ' +
		'exercitation ullamco laboris nisi aliquip ex ea'.split(' ')

	comma_marks := ['‚', '،', '，', ',', '﹐']
	sentence_marks := ['.', '…', '。', '!', '?']

	mut out := strings.new_builder(2048)

	for p in 0 .. cfg.paragraphs {
		sentence_count := vary(cfg.sentences_per_paragraph)

		for _ in 0 .. sentence_count {
			word_count := vary(cfg.words_per_sentence)

			expected_breaks := if cfg.words_per_sentence > 0 {
				(word_count * cfg.commas_per_sentence) / cfg.words_per_sentence
			} else {
				0
			}

			mut inserted_breaks := 0
			mut last_had_splitter := false
			min_dash_offset := 2

			for w in 0 .. word_count {
				word := lorem_words[rand.intn(lorem_words.len) or { 0 }]

				if w == 0 {
					out.write_string(capitalize(word))
				} else {
					out.write_string(' ')
					out.write_string(word)
				}

				is_last := w == word_count - 1

				// ---- Clause splitters
				if !is_last && inserted_breaks < expected_breaks
					&& (w + 1) * expected_breaks / word_count > inserted_breaks {
					use_strong := rand.intn(6) or { 0 } == 0

					if use_strong {
						if w >= min_dash_offset && w < word_count - 1 - min_dash_offset {
							out.write_string('—')
						} else {
							out.write_string(';')
						}
					} else {
						out.write_string(comma_marks[rand.intn(comma_marks.len) or { 0 }])
					}

					inserted_breaks++
					last_had_splitter = true
					continue
				}

				last_had_splitter = false

				// ---- Sentence terminator
				if is_last {
					if last_had_splitter {
						out.write_string('.')
					} else {
						out.write_string(sentence_marks[rand.intn(sentence_marks.len) or { 0 }])
					}
				}
			}

			out.write_string(' ')
		}

		if p < cfg.paragraphs - 1 {
			out.write_string('\n\n')
		}
	}

	return out.str()
}

// ---------------- Utilities --------------------

fn vary(base int) int {
	if base <= 1 {
		return 1
	}
	delta := base / 5
	r := rand.intn(delta * 2 + 1) or { 0 }
	return clamp(base + r - delta, 1, 10_000)
}

fn capitalize(s string) string {
	if s.len == 0 {
		return s
	}
	return s[..1].to_upper() + s[1..]
}

fn clamp(v int, min int, max int) int {
	if v < min {
		return min
	}
	if v > max {
		return max
	}
	return v
}

// ---------------- CLI --------------------------

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('lorem')
	fp.description('Lorem Ipsum generator with structured punctuation')
	fp.skip_executable()

	paragraphs := fp.int('paragraphs', `p`, 3, 'number of paragraphs')
	words := fp.int('words', `w`, 10, 'approx words per sentence')
	sentences := fp.int('sentences', `s`, 5, 'approx sentences per paragraph')
	commas := fp.int('commas', `c`, 2, 'comma density per sentence')
	seed := fp.int('seed', 0, 0, 'RNG seed (0 = deterministic default)')

	fp.finalize() or {
		eprintln(err)
		println(fp.usage())
		return
	}

	// Deterministic and portable seeding
	if seed != 0 {
		rand.seed([u32(seed), u32(seed ^ 0x9e3779b9)])
	} else {
		rand.seed([u32(0x12345678), 0x9abcdef0])
	}

	cfg := LoremCfg{
		paragraphs:              paragraphs
		words_per_sentence:      words
		sentences_per_paragraph: sentences
		commas_per_sentence:     commas
	}

	println(generate_lorem(cfg))
}
