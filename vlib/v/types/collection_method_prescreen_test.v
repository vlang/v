module types

import v.flat

fn register_prescreen_signature(mut tc TypeChecker, name string) {
	tc.fn_ret_types[name] = Type(int_)
	tc.register_generated_fn_param_types(name, []Type{})
}

// first_known_receiver_candidate is the unfiltered lookup that the prescreen replaces.
fn first_known_receiver_candidate(tc &TypeChecker, receiver Type, method string) ?string {
	for name in receiver_method_name_candidates(receiver, method, 'main') {
		if tc.fn_signature_known(name) && tc.method_can_be_called_on_receiver(receiver, method, name) {
			return name
		}
	}
	return none
}

fn prescreen_lookup(tc &TypeChecker, receiver Type, method string) ?string {
	assert method !in tc.collection_method_short_names
	mut recv := receiver
	if recv is Alias {
		recv = recv.base_type
	}
	return tc.selected_file_collection_method_without_collection_decl(receiver, recv,
		resolve_type_name_for_method(receiver), method)
}

fn test_collection_method_short_names_track_array_and_map_receivers() {
	mut tc := TypeChecker{}
	register_prescreen_signature(mut tc, '[]int.sum')
	register_prescreen_signature(mut tc, 'mymod.map[string]int.@lookup')
	register_prescreen_signature(mut tc, 'array.first_x')
	register_prescreen_signature(mut tc, 'Point.norm')
	assert 'sum' in tc.collection_method_short_names
	assert '@lookup' in tc.collection_method_short_names
	assert 'lookup' in tc.collection_method_short_names
	assert 'first_x' !in tc.collection_method_short_names
	assert 'norm' !in tc.collection_method_short_names
	tc.rebuild_fn_param_suffix_index()
	assert 'sum' in tc.collection_method_short_names
	assert 'lookup' in tc.collection_method_short_names
	assert 'first_x' !in tc.collection_method_short_names
}

fn test_collection_method_prescreen_matches_full_candidate_order() {
	mut tc := TypeChecker{}
	ints := Type(Array{
		elem_type: &Type(int_)
	})
	counts := Type(Map{
		key_type:   &Type(string_)
		value_type: &Type(int_)
	})
	aliased := Type(Alias{
		name_id:   flat.source_type_id('Ints')
		base_type: &Type(ints)
	})
	register_prescreen_signature(mut tc, 'array.first_x')
	register_prescreen_signature(mut tc, 'map.first_x')
	register_prescreen_signature(mut tc, 'Ints.first_x')
	register_prescreen_signature(mut tc, 'Ints.@only_escaped')
	register_prescreen_signature(mut tc, 'array.only_escaped')
	for receiver in [ints, counts, aliased] {
		for method in ['first_x', 'only_escaped', 'missing'] {
			want := first_known_receiver_candidate(&tc, receiver, method) or { '' }
			got := prescreen_lookup(&tc, receiver, method) or { '' }
			assert got == want, '${method} on ${receiver.name()}'
		}
	}
	assert (prescreen_lookup(&tc, aliased, 'first_x') or { '' }) == 'Ints.first_x'
	assert (prescreen_lookup(&tc, aliased, 'only_escaped') or { '' }) == 'Ints.@only_escaped'
	assert (prescreen_lookup(&tc, ints, 'only_escaped') or { '' }) == 'array.only_escaped'
	assert (prescreen_lookup(&tc, counts, 'first_x') or { '' }) == 'map.first_x'
}
