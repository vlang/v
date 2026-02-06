module xml

import strings

pub const default_entities = {
	'lt':   '<'
	'gt':   '>'
	'amp':  '&'
	'apos': "'"
	'quot': '"'
}

pub const default_entities_reverse = {
	'<': 'lt'
	'>': 'gt'
	'&': 'amp'
	"'": 'apos'
	'"': 'quot'
}

@[params]
pub struct EscapeConfig {
pub:
	reverse_entities map[string]string = default_entities_reverse
}

// escape_text replaces all entities in the given string with their respective
// XML entity strings. See default_entities, which can be overridden.
pub fn escape_text(content string, config EscapeConfig) string {
	mut flattened_entities := []string{cap: 2 * config.reverse_entities.len}

	for target, replacement in config.reverse_entities {
		flattened_entities << target
		flattened_entities << '&' + replacement + ';'
	}

	return content.replace_each(flattened_entities)
}

@[params]
pub struct UnescapeConfig {
pub:
	entities map[string]string = default_entities
}

// unescape_text replaces all entities in the given string with their respective
// original characters or strings. See default_entities_reverse, which can be overridden.
pub fn unescape_text(content string, config UnescapeConfig) !string {
	mut buffer := strings.new_builder(content.len)
	mut index := 0
	runes := content.runes()
	for index < runes.len {
		match runes[index] {
			`&` {
				mut offset := 1
				mut entity_buf := strings.new_builder(8)
				for index + offset < runes.len && runes[index + offset] != `;` {
					entity_buf.write_rune(runes[index + offset])
					offset++
				}
				// Did we reach the end of the string?
				if index + offset == runes.len {
					return error('Unexpected end of string while parsing entity.')
				}
				// Did we find a valid entity?
				entity := entity_buf.str()
				if entity in config.entities {
					buffer.write_string(config.entities[entity])
					index += offset
				} else {
					return error('Unknown entity: ' + entity)
				}
			}
			else {
				buffer.write_rune(runes[index])
			}
		}
		index++
	}
	return buffer.str()
}
