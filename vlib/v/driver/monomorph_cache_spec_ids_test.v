module driver

import v.flat
import v.transform

fn legacy_merge_monomorph_cache_specs(cached []transform.MonomorphCacheSpec, generated []transform.MonomorphCacheSpec) []transform.MonomorphCacheSpec {
	mut by_key := map[string]transform.MonomorphCacheSpec{}
	for specs in [cached, generated] {
		for spec in specs {
			key := '${spec.decl_key}\x00${spec.module}\x00${spec.args_text().join('\x1f')}'
			by_key[key] = spec
		}
	}
	mut keys := by_key.keys()
	keys.sort()
	mut merged := []transform.MonomorphCacheSpec{}
	for key in keys {
		merged << by_key[key]
	}
	return merged
}

fn test_merge_monomorph_cache_specs_uses_numeric_identity_and_legacy_output_order() {
	// Deliberately intern in the opposite order from the emitted spelling order.
	z_type := flat.source_type_id('SpecMergeZ')
	a_type := flat.source_type_id('SpecMergeA')
	mut specs := []transform.MonomorphCacheSpec{}
	for name in ['z', 'a'] {
		for mod in ['second', 'first'] {
			for ids in [[z_type], [a_type], [a_type, z_type], []u32{}] {
				specs << transform.MonomorphCacheSpec{
					decl_key: name
					module:   mod
					args:     flat.TypeTextList{ ids: ids }
				}
			}
		}
	}
	cached := specs.clone()
	mut generated := specs.clone()
	generated.reverse_in_place()
	assert merge_monomorph_cache_specs(cached, generated) == legacy_merge_monomorph_cache_specs(cached, generated)
	assert merge_monomorph_cache_specs([], []) == []transform.MonomorphCacheSpec{}
}

fn test_merge_monomorph_cache_specs_preserves_serialized_key_last_writer() {
	// The old output-key delimiter can also occur inside an interned spelling.
	// Preserve its last-writer rule at the serialization boundary.
	joined := transform.MonomorphCacheSpec{
		decl_key: 'same'
		module:   'same'
		args:     flat.TypeTextList.from_names(['SpecMergeA\x1fSpecMergeB'])
	}
	split := transform.MonomorphCacheSpec{
		decl_key: 'same'
		module:   'same'
		args:     flat.TypeTextList.from_names(['SpecMergeA', 'SpecMergeB'])
	}
	for specs in [[joined, split, joined], [split, joined, split]] {
		cached := specs[..2]
		generated := specs[2..]
		assert merge_monomorph_cache_specs(cached, generated) == legacy_merge_monomorph_cache_specs(cached, generated)
	}
}
