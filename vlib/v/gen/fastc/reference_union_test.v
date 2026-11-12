module fastc

import v.pref

fn test_reference_collection_unions_same_named_declarations() {
	prefs := pref.new_preferences()
	available := {
		'first':    true
		'second':   true
		'third':    true
		'separate': true
	}
	mut references := map[string]map[string]bool{}
	mut top_level := map[string]bool{}
	for index, source in [
		'module main\nfn (a First) shared_name() { first() }\n',
		'module main\nfn (b Second) shared_name() { second() }\nfn other() { separate() }\n',
		'module main\nfn (c Third) shared_name() { third() }\n',
	] {
		fastc_collect_file_references(FastcSourceFile{ path: 'source_${index}.v', source: source }, prefs,
			available, mut references, mut top_level)
	}
	assert references['shared_name'] == {
		'first':  true
		'second': true
		'third':  true
	}
	assert references['other'] == {
		'separate': true
	}
}

fn test_reference_partial_merge_keeps_independent_unions() {
	$if !v3_no_parallel ? {
		mut references := {
			'same': {
				'first': true
			}
		}
		mut top_level := map[string]bool{}
		chunk := FastcReferencePartial{
			references:           {
				'same':  {
					'second': true
				}
				'other': {
					'third': true
				}
			}
			top_level_references: {
				'initializer': true
			}
		}
		fastc_merge_reference_partial(chunk, mut references, mut top_level)
		assert references['same'] == {
			'first':  true
			'second': true
		}
		assert references['other'] == {
			'third': true
		}
		assert top_level == {
			'initializer': true
		}
		references['same']['later'] = true
		assert chunk.references['same'] == {
			'second': true
		}
	}
}
