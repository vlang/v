import regex.pcre

// Since 2026/02/08, regex.pcre is built-in and implemented in pure V (VM Edition).
// This version uses `compile` and direct struct access for match results.

fn example() {
	// 1. Use pcre.compile() which returns a result (!)
	r := pcre.compile('Match everything after this: (.+)') or {
		println('An error occurred: ${err}')
		return
	}

	// 2. Use r.find() to search for the pattern. 
	// Returns an option (?) containing a Match struct.
	m := r.find('Match everything after this: "I ❤️ VLang!"') or {
		println('No match!')
		return
	}

	// In the new module:
	// m.text      -> Corresponds to the whole match (formerly m.get(0))
	// m.groups[0] -> Corresponds to the first capturing group (formerly m.get(1))

	whole_match := m.text
	
	// Check if group exists before accessing
	if m.groups.len > 0 {
		matched_captured := m.groups[0]
		println(whole_match)      // Match everything after this: "I ❤️ VLang!"
		println(matched_captured) // "I ❤️ VLang!"
	}
}

fn main() {
	example()

	text := '[ an s. s! ]( wi4ki:something )
	[ an s. s! ]( wi4ki:something )
	[ an s. s! ](wiki:something)
	[ an s. s! ](something)dd
	d [ an s. s! ](something ) d
	[  more text ]( something ) s [ something b ](something)dd'

	// Regex for markdown-style links
	regex_str := r'(\[[a-z\.\! ]*\]\( *\w*\:*\w* *\))*'

	r := pcre.compile(regex_str) or {
		println('An error occurred during compile!')
		return
	}

	// find() scans the text for the first occurrence
	m := r.find(text) or {
		println('No match found in text block!')
		return
	}

	// Print the full match found
	println('Whole match found:')
	println(m.text)

	// Print all captured groups
	println('Groups found: ${m.groups}')
	
	// If you want to find ALL matches in the text instead of just the first:
	println('\nFinding all non-overlapping matches:')
	all_matches := r.find_all(text)
	for i, mt in all_matches {
		if mt.text.trim_space() != '' {
			println('${i}: ${mt.text.trim_space()}')
		}
	}
}