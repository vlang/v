// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os

fn main() {
	mut path := 'cinderella.txt'
	if os.args.len != 2 {
		println('usage: word_counter [text_file]')
		println('using ${path}')
	} else {
		path = os.args[1]
	}
	contents := os.read_file(path.trim_space()) or {
		println('failed to open ${path}')
		return
	}
	mut m := map[string]int{}
	for word in extract_words(contents) {
		m[word]++
	}
	// Sort the keys
	mut keys := m.keys()
	keys.sort()
	// Print the map
	for key in keys {
		val := m[key]
		println('${key} => ${val}')
	}
}

// Creates an array of words from a given string
fn extract_words(contents string) []string {
	mut splits := []string{}
	for space_split in contents.to_lower().split(' ') {
		if space_split.contains('\n') {
			splits << space_split.split('\n')
		} else {
			splits << space_split
		}
	}

	mut results := []string{}
	for s in splits {
		result := filter_word(s)
		if result == '' {
			continue
		}
		results << result
	}

	return results
}

// Removes punctuation
fn filter_word(word string) string {
	if word == '' || word == ' ' {
		return ''
	}
	mut i := 0
	for i < word.len && !word[i].is_letter() {
		i++
	}
	start := i
	for i < word.len && word[i].is_letter() {
		i++
	}
	end := i
	return word[start..end]
}
