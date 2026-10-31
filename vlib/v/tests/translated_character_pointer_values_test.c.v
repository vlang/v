@[translated]
module main

struct CharacterPointerHolder {
	data &i8
}

fn duplicate_translated_chars(text &i8) &i8 {
	return C.strdup(text)
}

fn read_translated_chars(text &i8) i8 {
	return *text
}

fn choose_translated_chars(kind int, text &i8) &i8 {
	chosen := if kind == 0 { text } else { C.strdup(c'if') }
	matched := match kind {
		0 { text }
		else { C.strdup(c'match') }
	}
	return if kind == 1 { chosen } else { matched }
}

fn test_translated_character_pointer_values() {
	text := C.strdup(c'hello')
	duplicate := duplicate_translated_chars(&i8(text))
	defer {
		unsafe {
			C.free(text)
			C.free(duplicate)
		}
	}
	holder := CharacterPointerHolder{ data: text }
	values := [&i8(text), text]
	mut appended := []&i8{}
	appended << text
	mapping := {
		'text':      &i8(text)
		'duplicate': C.strdup(text)
	}
	mapped := mapping['duplicate'] or { panic('missing duplicate') }
	defer { unsafe { C.free(mapped) } }
	assert read_translated_chars(text) == 104
	assert *duplicate == 104
	assert *holder.data == 104
	assert *values[1] == 104
	assert *appended[0] == 104
	assert *mapped == 104
	assert *choose_translated_chars(0, text) == 104
	assert *choose_translated_chars(1, text) == 105
	assert *choose_translated_chars(2, text) == 109
}
