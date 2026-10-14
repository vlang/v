module json2

// Doc is a parsed JSON document. Use `value`, `value_opt` or `decode` to read
// from it.
pub struct Doc {
	params DecoderOptions
pub:
	root Any
}

// new_doc wraps an already parsed `Any` tree.
pub fn new_doc(root Any, params DecoderOptions) Doc {
	return Doc{
		root:   root
		params: params
	}
}

// to_any returns the document root as a dynamic value.
pub fn (d Doc) to_any() Any {
	return d.root
}

// str returns the document rendered in the dialect it was parsed with.
pub fn (d Doc) str() string {
	return encode(d.root, json5: d.params.json5)
}

// value queries a value with a small path syntax: dots separate object keys,
// brackets index arrays, and a key that contains a dot can be quoted, as in
// `a."b.c"`. Returns `Null` when the path does not resolve.
pub fn (d Doc) value(key string) Any {
	return resolve_path(d.root, parse_path(key) or { return null })
}

// value_opt queries a value and returns an error when the path does not
// resolve, instead of `Null`.
pub fn (d Doc) value_opt(key string) !Any {
	steps := parse_path(key) or { return error('invalid path `${key}`') }
	return resolve_path_opt(d.root, steps) or { return error('json2: no key `${key}`') }
}

// get queries a value and returns an option, so `none` distinguishes a missing
// key from an explicit `null`.
pub fn (d Doc) get(key string) ?Any {
	steps := parse_path(key) or { return none }
	return resolve_path_opt(d.root, steps)
}

// decode decodes the document into `T`. It is the method form of the
// package-level `decode` and shares its attribute and hook support.
pub fn (d Doc) decode[T]() !T {
	return decode_any[T](d.root, d.params)
}

// reflect sets the fields of `T` from the matching document keys, leaving
// absent fields at their default value. Fields are matched by name or by a
// `@[json: 'name']` attribute. Unlike `decode`, a key that cannot be decoded
// into its field is silently skipped instead of reported, so a partially
// matching document still fills in what it can.
pub fn (d Doc) reflect[T]() T {
	mut out := T{}
	reflect_into(d.root, mut out, d.params)
	return out
}

// PathStep is one element of a parsed lookup path: either an object key or an
// array index.
pub struct PathStep {
pub:
	key   string
	index int
}

// is_index reports whether the step indexes an array.
pub fn (s PathStep) is_index() bool {
	return s.index >= 0
}

// str returns the step in path syntax.
pub fn (s PathStep) str() string {
	return if s.is_index() { '[${s.index}]' } else { s.key }
}

// parse_path splits a lookup path into steps. Quoted segments keep any dots
// they contain, so `a."b.c"` addresses the key `b.c` inside the object `a`.
//
// The path is walked with an explicit index rather than a state machine, which
// keeps the "flush the pending key" logic in one place.
pub fn parse_path(path string) ![]PathStep {
	mut steps := []PathStep{}
	runes := path.runes()
	mut i := 0
	mut key := []rune{}
	mut has_key := false
	for i < runes.len {
		ch := runes[i]
		match ch {
			`.` {
				push_key(mut steps, key, has_key)
				key = []rune{}
				has_key = false
				i++
			}
			`[` {
				push_key(mut steps, key, has_key)
				key = []rune{}
				has_key = false
				mut j := i + 1
				mut digits := []rune{}
				for j < runes.len && json5_is_digit(int(runes[j])) {
					digits << runes[j]
					j++
				}
				if digits.len == 0 || j >= runes.len || runes[j] != `]` {
					return error('expected an array index like `[0]` in path `${path}`')
				}
				steps << PathStep{
					key:   ''
					index: digits.string().int()
				}
				i = j + 1
			}
			`'`, `"` {
				if has_key {
					return error('unexpected quote in path `${path}`')
				}
				mut j := i + 1
				mut quoted := []rune{}
				for j < runes.len && runes[j] != ch {
					quoted << runes[j]
					j++
				}
				if j >= runes.len {
					return error('unterminated quoted key in path `${path}`')
				}
				steps << PathStep{
					key:   quoted.string()
					index: -1
				}
				has_key = false
				key = []rune{}
				i = j + 1
				if i < runes.len && runes[i] != `.` && runes[i] != `[` {
					return error('unexpected character after a quoted key in path `${path}`')
				}
			}
			else {
				key << ch
				has_key = true
				i++
			}
		}
	}
	push_key(mut steps, key, has_key)
	return steps
}

// push_key appends a pending object-key step to `mut steps` when there is one.
fn push_key(mut steps []PathStep, key []rune, has_key bool) {
	if has_key {
		steps << PathStep{
			key:   key.string()
			index: -1
		}
	}
}

// resolve_path walks steps through value, returning null when a step does not resolve.
pub fn resolve_path(value Any, steps []PathStep) Any {
	return resolve_path_opt(value, steps) or { null }
}

// The traversal borrows containers from the document; autofree must not release them.
@[manualfree]
fn resolve_path_opt(value Any, steps []PathStep) ?Any {
	mut current := value
	for step in steps {
		if step.is_index() {
			if current is []Any {
				if step.index >= current.len {
					return none
				}
				current = current[step.index]
				continue
			}
			return none
		}
		if current is map[string]Any {
			current = current[step.key] or { return none }
		} else {
			return none
		}
	}
	return current
}

// reflect_into fills the fields of `mut out` from `value`, skipping any key it
// cannot decode.
@[manualfree]
fn reflect_into[T](value Any, mut out T, params DecoderOptions) {
	$if T is $struct {
		obj := match value {
			map[string]Any { value }
			else { return }
		}
		// `$for` unrolls to straight-line code, so the field logic is nested
		// `if`s rather than a loop with `continue`.
		$for field in T.fields {
			key := field_key_name(field.name, field.attrs)
			if key != '' {
				$if field.is_embed {
					$if field.unaliased_typ is $struct {
						mut embedded := out.$(field.name)
						reflect_into(value, mut embedded, params)
						out.$(field.name) = embedded
					}
				} $else {
					// A key that is absent, `null`, or of a kind the field cannot
					// hold leaves the field at its default.
					if item := obj[key] {
						if item !is Null {
							mut slot := out.$(field.name)
							reflect_value(item, mut slot, params)
							out.$(field.name) = slot
						}
					}
				}
			}
		}
	} $else {
		reflect_value(value, mut out, params)
	}
}

@[manualfree]
fn reflect_value[T](value Any, mut out T, params DecoderOptions) {
	if decoded := decode_any[T](value, params) {
		out = decoded
	}
}

fn field_key_name(name string, attrs []string) string {
	info := encoder_field_info(name, attrs)
	return if info.is_skip { '' } else { info.key_name }
}
