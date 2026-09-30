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
}
