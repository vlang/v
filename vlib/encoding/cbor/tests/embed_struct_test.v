import encoding.cbor
import encoding.hex

struct Inner {
	a int
}

struct Outer {
	Inner
	b int
}

fn test_decode_flattened_embed() {
	bytes := hex.decode('a2616101616202')!
	result := cbor.decode[Outer](bytes, cbor.DecodeOpts{ deny_unknown_fields: true })!
	assert result.a == 1
	assert result.b == 2
}

fn test_nested_embed_wire_format_is_preserved() {
	original := Outer{Inner{1}, 2}
	bytes := cbor.encode(original, cbor.EncodeOpts{})!
	assert bytes.hex() == 'a265496e6e6572a1616101616202'
	assert cbor.decode[Outer](bytes, cbor.DecodeOpts{})! == original
	canonical := cbor.encode(original, cbor.EncodeOpts{ canonical: true })!
	assert canonical.hex() == 'a261620265496e6e6572a1616101'
	assert cbor.decode[Outer](canonical, cbor.DecodeOpts{})! == original
}

struct Middle {
	Inner
	m string
}

struct Deep {
	Middle
	z bool
}

fn test_multilevel_embeds_and_partial_nesting() {
	flat := hex.decode('a3616103616d6178617af5')!
	value := cbor.decode[Deep](flat, cbor.DecodeOpts{ deny_unknown_fields: true })!
	assert value.a == 3
	assert value.m == 'x'
	assert value.z
	assert cbor.decode[Deep](cbor.encode(value, cbor.EncodeOpts{})!, cbor.DecodeOpts{})! == value
	// A nested Inner is also visible through the flattened Middle.
	partial := hex.decode('a365496e6e6572a1616103616d6178617af5')!
	assert cbor.decode[Deep](partial, cbor.DecodeOpts{ deny_unknown_fields: true })! == value
}

struct Shadow {
	Inner
	a int
}

fn test_outer_fields_shadow_promoted_fields() {
	bytes := hex.decode('a265496e6e6572a1616109616102')!
	value := cbor.decode[Shadow](bytes, cbor.DecodeOpts{})!
	assert value.a == 2
	assert value.Inner.a == 9
}

fn test_mixed_nested_and_flat_fields_follow_wire_order() {
	for nested_first in [false, true] {
		mut p := cbor.new_packer(cbor.EncodeOpts{})
		p.pack_map_header(2)
		if !nested_first {
			p.pack_text('a')
			p.pack_int(1)
		}
		p.pack_text('Inner')
		p.pack(Inner{9})!
		if nested_first {
			p.pack_text('a')
			p.pack_int(1)
		}
		value := cbor.decode[Outer](p.bytes(), cbor.DecodeOpts{})!
		assert value.a == if nested_first { 1 } else { 9 }
	}
}

@[cbor_rename_all: 'kebab-case']
struct Details {
	long_name string
	count     int @[cbor: 'n']
	ignored   int @[skip]
	values    []int
	labels    map[string]string
	maybe     ?int
}

struct Detailed {
	Details
	b int
}

struct FlatDetails {
	long_name string @[cbor: 'long-name']
	n         int
	values    []int
	labels    map[string]string
	maybe     ?int
	b         int
}

fn test_flat_embedded_attributes_options_and_collections() {
	for optional in [?int(none), ?int(5)] {
		flat := FlatDetails{
			long_name: 'name'
			n:         7
			values:    [1, 2]
			labels:    {
				'key': 'value'
			}
			maybe:     optional
			b:         4
		}
		bytes := cbor.encode(flat, cbor.EncodeOpts{})!
		value := cbor.decode[Detailed](bytes, cbor.DecodeOpts{ deny_unknown_fields: true })!
		assert value.long_name == 'name'
		assert value.count == 7
		assert value.ignored == 0
		assert value.values == [1, 2]
		assert value.labels == {
			'key': 'value'
		}
		assert value.maybe == optional
		assert value.b == 4
	}
}

struct SkippedEmbed {
	Inner  @[skip]
	b     int
}

struct OrdinaryNested {
	inner Inner
	b     int
}

fn test_nonembedded_and_skipped_fields_are_not_flattened() {
	bytes := hex.decode('a2616101616202')!
	assert cbor.decode[SkippedEmbed](bytes, cbor.DecodeOpts{})!.a == 0
	assert cbor.decode[OrdinaryNested](bytes, cbor.DecodeOpts{})!.inner.a == 0
	if _ := cbor.decode[SkippedEmbed](bytes, cbor.DecodeOpts{ deny_unknown_fields: true }) {
		assert false
	}
	if _ := cbor.decode[OrdinaryNested](bytes, cbor.DecodeOpts{ deny_unknown_fields: true }) {
		assert false
	}
	assert cbor.decode[OrdinaryNested](bytes, cbor.DecodeOpts{})!.b == 2

	ordinary := OrdinaryNested{Inner{3}, 5}
	assert cbor.decode[OrdinaryNested](cbor.encode(ordinary, cbor.EncodeOpts{})!, cbor.DecodeOpts{})! == ordinary
}

fn test_flattened_embed_strict_errors_and_indefinite_maps() {
	indefinite := hex.decode('bf616101616202ff')!
	assert cbor.decode[Outer](indefinite, cbor.DecodeOpts{ deny_unknown_fields: true })!.a == 1
	for wire in ['a2616101616102', 'a161616178', 'a1616303'] {
		bytes := hex.decode(wire)!
		if _ := cbor.decode[Outer](bytes, cbor.DecodeOpts{
			deny_duplicate_keys: true
			deny_unknown_fields: true
		}) {
			assert false, 'expected strict decode to reject ${wire}'
		}
	}
}

struct OtherInner {
	a int
}

struct AmbiguousEmbeds {
	Inner
	OtherInner
}

fn test_first_declared_embed_wins_ambiguous_promoted_names() {
	bytes := hex.decode('a1616107')!
	value := cbor.decode[AmbiguousEmbeds](bytes, cbor.DecodeOpts{})!
	assert value.Inner.a == 7
	assert value.OtherInner.a == 0
}
