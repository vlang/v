module json

import time

struct Node {
	key_pos  int
	key_len  int
	children ?[]Node
}

struct Decoder {
	json_data string
mut:
	idx int
}

// check_json
fn check_json(val string) ! {
	if val.len == 0 {
		return error('empty string')
	}
}

// decode
pub fn decode[T](val string) !T {
	check_json(val)!

	mut nodes := []Node{}

	mut decoder := Decoder{
		json_data: val
	}

	// TODO needs performance improvements
	decoder.fulfill_nodes(mut nodes)

	mut result := T{}

	decoder.decode_value(nodes, &result)
	return result
}

// decode_value
fn (mut decoder Decoder) decode_value[T](nodes []Node, val &T) {
	$if val is $option {
		workaround := val
		if workaround != none {
			decoder.decode_value(nodes, workaround)
		}
	} $else $if T is string {
	} $else $if T is $sumtype {
		$for v in val.variants {
			if val is v {
				decoder.decode_value(nodes, val)
			}
		}
	} $else $if T is $alias {
	} $else $if T is time.Time {
	} $else $if T is $map {
	} $else $if T is $array {
	} $else $if T is $struct {
		decoder.decode_struct(nodes, val)
	} $else $if T is $enum {
	} $else $if T is $int {
	} $else $if T is $float {
	} $else $if T is bool {
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

// decode_struct
fn (mut decoder Decoder) decode_struct[T](nodes []Node, value &T) {
	$for field in T.fields {
		for i := 0; i < nodes.len; i++ {
			mut node := nodes[i]

			if node.key_len == field.name.len {
				if unsafe {
					vmemcmp(decoder.json_data.str + node.key_pos, field.name.str, field.name.len) == 0
				} {
					$if field.typ is string {
						start := (node.key_pos + node.key_len) + 4
						mut end := start
						for {
							if decoder.json_data[end] == `"` {
								if decoder.json_data[end + 1] == `,`
									|| decoder.json_data[end + 1] == `}` {
									break
								}
							}
							end++
						}
						value.$(field.name) = decoder.json_data[start..end]
					} $else $if field.typ is $int {
						start := (node.key_pos + node.key_len) + 3
						mut end := start
						for {
							if decoder.json_data[end] == `,` || decoder.json_data[end] == `}` {
								break
							}

							end++
						}
						value.$(field.name) = decoder.json_data[start..end].int()
					} $else $if T is bool {
					} $else $if T is time.Time {
					} $else $if T is $struct {
						if node.children != none {
							decoder.decode_value(node.children or { panic('It will never happens') },
								value.$(field.name))
						}
					} $else {
						eprintln('not supported')
					}
					break
				}
			}
		}
	}
}

// fulfill_nodes
fn (mut decoder Decoder) fulfill_nodes(mut nodes []Node) {
	mut inside_string := false
	mut inside_key := false

	mut actual_key_len := 0
	for decoder.idx < decoder.json_data.len {
		letter := decoder.json_data[decoder.idx]
		if letter == ` ` && !inside_string {
		} else if letter == `\"` {
			if decoder.json_data[decoder.idx - 1] == `{`
				|| decoder.json_data[decoder.idx - 2] == `,` {
				inside_key = true
			} else if decoder.json_data[decoder.idx + 1] == `:` {
				if decoder.json_data[decoder.idx + 3] == `{` {
					mut children := []Node{}
					key_pos := decoder.idx - actual_key_len
					key_len := actual_key_len

					decoder.idx += 3
					decoder.fulfill_nodes(mut children)

					nodes << Node{
						key_pos: key_pos
						key_len: key_len
						children: children
					}
				} else {
					nodes << Node{
						key_pos: decoder.idx - actual_key_len
						key_len: actual_key_len
					}
				}

				inside_key = false
			}
			inside_string = !inside_string
			decoder.idx++
			continue
		} else if letter == `:` {
			actual_key_len = 0
		} else if letter == `,` || letter == `:` || letter == `{` || letter == `}` || letter == `[`
			|| letter == `]` {
		} else {
		}
		if inside_key {
			actual_key_len++
		}
		decoder.idx++
	}
}
