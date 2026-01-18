module strings

fn test_lorem_generate_basic() {
	output := lorum(LoremCfg{
		paragraphs:              2
		sentences_per_paragraph: 3
		words_per_sentence:      5
	})
	assert output.len > 0
	// 2 paragraphs should be separated by "\n\n"
	assert output.count('\n\n') == 1
}

fn test_lorem_generate_deterministic() {
	cfg := LoremCfg{
		rng_seed:   12345
		paragraphs: 1
	}
	out1 := lorum(cfg)
	out2 := lorum(cfg)
	assert out1 == out2
}

fn test_lorem_generate_counts() {
	cfg := LoremCfg{
		paragraphs: 3
	}
	output := lorum(cfg)
	// There should be 2 separators for 3 paragraphs
	assert output.count('\n\n') == 2
}

fn test_lorem_custom_corpus() {
	// 'bard' is shakespeare
	cfg := LoremCfg{
		corpus_name: 'bard'
		rng_seed:    999
		paragraphs:  1
	}
	output := lorum(cfg)
	assert output.len > 0
	// Hard to check exact content due to randomness, but it should not crash
}

fn test_lorem_vary() {
	// lorem_vary is private, effectively testing internal logic
	// base 10, min 5
	// delta = 10 * 0.2 = 2
	// range = [-2, 2]
	// result = 10 + [-2, 2] -> [8, 12]
	// We can't guarantee a specific value, but we can check bounds
	mut rng := LorumRNG{
		seed: 12345
	}
	for _ in 0 .. 100 {
		val := lorem_vary(mut rng, 10, 5)
		assert val >= 8
		assert val <= 12
	}
}

fn test_lorem_tokenize() {
	text := 'Hello\nWorld  Test'
	tokens := lorem_tokenize(text)
	assert tokens == ['Hello', 'World', 'Test']
}
