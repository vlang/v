// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import os

fn main() {
	mut path = 'cinderella.txt'
	if os.args.len != 2 {
		println('usage: word_counter [text_file]')
		println('using $path')
	}
	else {
		path = os.args[1]
	}
	contents := os.read_file(path.trim_space())
	mut m := map[string]int{}
	for word in contents.to_lower().split(' ') {
		key := filter_word(word)
		if key == '' {
			continue
		}
		m[key] = m[key] + 1// TODO m[key]++
	}
	// Sort the keys
	mut keys := []string
	for e in m.entries {
		keys << e.key
	}
	keys.sort()
	// Print the map
	for key in keys {
		val := m[key]
		println('$key => $val')
	}
}

// Removes punctuation
fn filter_word(word string) string {
	if word == '' || word == ' ' {
		return ''
	}
	mut i := 0
	for i < word.len && !is_letter(word[i]) {
		i++
	}
	start := i
	for i < word.len && is_letter(word[i]) {
		i++
	}
	end := i
	return word.substr(start, end)
}

// TODO remove once it's possible to call word[i].is_letter()
fn is_letter(c byte) bool {
	return c.is_letter()
}

