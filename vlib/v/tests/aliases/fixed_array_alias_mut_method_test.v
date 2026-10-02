type ResetWords = [4]u32

fn (mut words ResetWords) reset() {
	for i in 0 .. words.len {
		words[i] = 0
	}
}

fn (words ResetWords) clone() ResetWords {
	return words
}

fn test_fixed_array_alias_mut_method_keeps_original_storage() {
	original := ResetWords([u32(1), 2, 3, 4]!)
	mut words := original.clone()
	words.reset()
	assert words == ResetWords{}
	assert original == ResetWords([u32(1), 2, 3, 4]!)
}
