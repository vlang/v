module strict

import arrays

pub struct KeyStruct {
pub:
	key        string
	value_type KeyType
	token_pos  int // the position of the token
}

pub enum KeyType {
	literal
	map
	array
}

pub struct StructCheckResult {
pub:
	duplicates  []string
	superfluous []string
}

// strict_check .
pub fn strict_check[T](json_data string) StructCheckResult {
	// REVIEW how performatic is it?
	$if T is $struct {
		tokens := tokenize(json_data)

		key_struct := get_keys_from_json(tokens)

		mut duplicates := get_duplicate_keys_from_json(tokens)
		mut superfluous := get_superfluous_keys[T](key_struct)

		mut val := T{}

		$for field in T.fields {
			$if field.typ is $struct {
				if last_key := find_last_key(key_struct, field.name) {
					// TODO: get path here from `last_key.key`
					if last_key.value_type == .map {
						check(val.$(field.name), tokens[last_key.token_pos + 2..], mut superfluous)
					}
				}
			}
		}
		return StructCheckResult{
			duplicates:  duplicates
			superfluous: superfluous
		}
	} $else {
		return StructCheckResult{}
	}
}

fn check[T](val T, tokens []string, mut superfluous []string) {
	$if T is $struct {
		key_struct := get_keys_from_json(tokens)

		for unnecessary in get_superfluous_keys[T](key_struct) {
			superfluous << unnecessary
		}

		$for field in T.fields {
			$if field.typ is $struct {
				if last_key := find_last_key(key_struct, field.name) {
					if last_key.value_type == .map {
						check(val.$(field.name), tokens[last_key.token_pos + 2..], mut superfluous)
					}
				}
			}
		}
	}
}

fn get_superfluous_keys[T](key_struct []KeyStruct) []string {
	mut superfluous := []string{}

	struct_keys := get_keys_from_[T]()

	json_keys := key_struct.map(it.key)

	for json_key in json_keys {
		if !struct_keys.contains(json_key) {
			superfluous << json_key
		}
	}
	return superfluous
}

fn get_duplicate_keys(keys []string) []string {
	return arrays.uniq_only_repeated(keys.sorted())
}

fn get_duplicate_keys_from_json(tokens []string) []string {
	mut object_key_stack := [][]string{}
	mut duplicates := []string{}
	for i, token in tokens {
		match token {
			'{' {
				object_key_stack << []string{}
			}
			'}' {
				if object_key_stack.len == 0 {
					continue
				}
				current_keys := object_key_stack.pop()
				for duplicate in get_duplicate_keys(current_keys) {
					if !duplicates.contains(duplicate) {
						duplicates << duplicate
					}
				}
			}
			':' {
				if object_key_stack.len == 0 || i == 0 {
					continue
				}
				last_idx := object_key_stack.len - 1
				object_key_stack[last_idx] << tokens[i - 1].replace('"', '')
			}
			else {}
		}
	}
	return duplicates
}

fn find_last_key(key_struct []KeyStruct, field_name string) ?KeyStruct {
	for idx := key_struct.len; idx > 0; idx-- {
		item := key_struct[idx - 1]
		if item.key == field_name {
			return item
		}
	}
	return none
}

fn get_keys_from_[T]() []string {
	mut struct_keys := []string{}
	$if T is $struct {
		$for field in T.fields {
			struct_keys << field.name
		}
	}
	return struct_keys
}

// get_keys_from_json .
pub fn get_keys_from_json(tokens []string) []KeyStruct {
	mut key_structs := []KeyStruct{}

	mut object_depth := 0
	mut array_depth := 0
	mut found_root_object := false
	for i, token in tokens {
		match token {
			'{' {
				object_depth++
				found_root_object = true
			}
			'}' {
				if object_depth > 0 {
					object_depth--
				}
				if found_root_object && object_depth == 0 {
					break
				}
			}
			'[' {
				array_depth++
			}
			']' {
				if array_depth > 0 {
					array_depth--
				}
			}
			':' {
				if object_depth != 1 || array_depth != 0 || i == 0 || i + 1 >= tokens.len {
					continue
				}
				current_key := tokens[i - 1].replace('"', '')
				key_type := if tokens[i + 1] == '{' {
					KeyType.map
				} else if tokens[i + 1] == '[' {
					KeyType.array
				} else {
					KeyType.literal
				}
				key_structs << KeyStruct{
					key:        current_key
					value_type: key_type
					token_pos:  i - 1
				}
			}
			else {}
		}
	}

	return key_structs
}

fn tokenize(json_data string) []string {
	mut tokens := []string{}
	mut current_token := ''
	mut inside_string := false
	json_without_newlines := json_data.replace('\n', ' ')
	normalized_json := json_without_newlines.replace('\t', ' ')

	for letter in normalized_json {
		if letter == ` ` && !inside_string {
			if current_token != '' {
				tokens << current_token
				current_token = ''
			}
		} else if letter == `\"` {
			inside_string = !inside_string
			current_token += '"'
		} else if letter == `,` || letter == `:` || letter == `{` || letter == `}` || letter == `[`
			|| letter == `]` {
			if current_token != '' {
				tokens << current_token
				current_token = ''
			}
			tokens << [letter].bytestr()
		} else {
			current_token += [letter].bytestr()
		}
	}

	if current_token != '' {
		tokens << current_token
	}
	return tokens
}
