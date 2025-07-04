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

		mut duplicates := get_duplicates_keys(key_struct)
		mut superfluous := get_superfluous_keys[T](key_struct)

		mut val := T{}

		$for field in T.fields {
			$if field.typ is $struct {
				field_name := field.name
				last_key := arrays.find_last(key_struct, fn [field_name] (k KeyStruct) bool {
					return k.key == field_name
				}) or { panic('field not found: ' + field.name) }

				// TODO: get path here from `last_key.key`
				if last_key.value_type == .map {
					check(val.$(field.name), tokens[last_key.token_pos + 2..], mut duplicates, mut
						superfluous)
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

fn check[T](val T, tokens []string, mut duplicates []string, mut superfluous []string) {
	$if T is $struct {
		key_struct := get_keys_from_json(tokens)

		for duplicate in get_duplicates_keys(key_struct) {
			duplicates << duplicate
		}

		for unnecessary in get_superfluous_keys[T](key_struct) {
			superfluous << unnecessary
		}

		$for field in T.fields {
			$if field.typ is $struct {
				if last_key.value_type == .map {
					check(val.$(field.name), tokens[last_key.token_pos + 2..], mut duplicates, mut
						superfluous)
				}
			}
		}
	}
}

fn get_superfluous_keys[T](key_struct []KeyStruct) []string {
	mut superfluous := []string{}

	struct_keys := get_keys_from_[T]()

	json_keys := key_struct.map(fn (json_key KeyStruct) string {
		return json_key.key
	})

	for json_key in json_keys {
		if !struct_keys.contains(json_key) {
			superfluous << json_key
		}
	}
	return superfluous
}

fn get_duplicates_keys(key_struct []KeyStruct) []string {
	json_keys := key_struct.map(it.key).sorted()
	return arrays.uniq_only_repeated(json_keys)
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

	mut nested_map_count := 0

	for i, token in tokens {
		if token == ':' {
			mut current_key := tokens[i - 1].replace('"', '')
			if tokens[i + 1] == '{' {
				if nested_map_count == 0 {
					key_type := KeyType.map
					key_structs << KeyStruct{
						key:        current_key
						value_type: key_type
						token_pos:  i - 1
					}
				}
				nested_map_count++
			} else if tokens[i + 1] == '[' {
				continue
			} else if nested_map_count > 0 {
				if tokens[i + 1] == '}' {
					nested_map_count--
				} else {
					// REVIEW Não sei
				}
			} else {
				key_type := KeyType.literal
				key_structs << KeyStruct{
					key:        current_key
					value_type: key_type
					token_pos:  i - 1
				}
			}
		}
	}

	return key_structs
}

fn tokenize(json_data string) []string {
	mut tokens := []string{}
	mut current_token := ''
	mut inside_string := false

	for letter in json_data.replace('\n', ' ').replace('\t', ' ') {
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
