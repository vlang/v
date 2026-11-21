// Tests that toml.encode and map[string]Any.to_toml quote non-bare keys,
// so that `decode(encode(x)) == x` holds for keys that are not
// `[A-Za-z0-9_-]+` (see vlang/v#31880).
import toml

struct Holder {
	nested Embedded
}

struct Embedded {
	inner map[string]toml.Any
}

fn test_encode_quotes_keys_with_special_characters() {
	mp := map[string]toml.Any{
		'x#y':  toml.Any(1)
		'a.b':  toml.Any(2)
		'has"': toml.Any(3)
		'a b':  toml.Any(4)
	}
	text := mp.to_toml()

	// map iteration order is unspecified, so every expected form must be
	// present; the quote key is emitted escaped:
	assert text.contains('"a.b" = 2')
	assert text.contains('"x#y" = 1')
	assert text.contains('"a b" = 4')
	assert text.contains('"has\\"" = 3')
}

fn test_inline_toml_quotes_non_bare_keys() {
	mp := map[string]toml.Any{
		'a.b': toml.Any(7)
	}
	text := mp.to_inline_toml()
	assert text == '{ "a.b" = 7 }'
}

fn test_bare_keys_are_not_quoted() {
	mp := map[string]toml.Any{
		'leaf':   toml.Any(5)
		'-dash':  toml.Any(6)
		'9lives': toml.Any(7)
	}
	text := mp.to_toml()
	assert text.contains('leaf = 5')
	assert text.contains('-dash = 6')
	assert text.contains('9lives = 7')
}

fn test_struct_round_trip_preserves_dotted_keys() {
	src_key := 'a.b'
	h := Holder{
		nested: Embedded{
			inner: map[string]toml.Any{
				src_key: toml.Any(7)
			}
		}
	}
	enc := toml.encode(h)
	assert enc.contains('"a.b" = 7')
	d := toml.decode[Holder](enc) or { panic('redecode failed: ${err.msg()}') }
	inner := d.nested.inner
	assert inner.len == 1
	mut keys := []string{}
	for k, _ in inner {
		keys << k
	}
	assert keys == [src_key]
}
