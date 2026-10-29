module fastc

import v.flat

fn test_fastc_constant_output_survives_initializer_scope_release() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		mut types := flat.TypeTextBindings{}
		types.set('answer'.clone(), 'int')
		mut fixed := flat.TypeTextBindings{}
		fixed.set('FixedArray_2_int'.clone(), 'FixedArray_2_int')
		result := FastcConstantFileResult{
			used_field_defaults: true
			values:              [FastcConstantValue{
				key:          'answer'.clone()
				c_name:       'main__answer'.clone()
				module_name:  'main'.clone()
				value:        'base + 1'.clone()
				typ:          flat.source_type_int
				dependencies: ['base'.clone()]
				is_runtime:   true
			}]
			constant_types:      types
			fixed_array_types:   fixed
			composite_types:     {
				'Array_int'.clone(): true
			}
			error_message:       'initializer error'.clone()
		}
		unsafe { prealloc_scope_leave(scope) }
		owned := fastc_clone_constant_file_result(result)
		assert !unsafe { prealloc_scope_owns(scope, owned.values.data) }
		assert !unsafe { prealloc_scope_owns(scope, owned.values[0].key.str) }
		assert !unsafe { prealloc_scope_owns(scope, owned.values[0].value.str) }
		assert !unsafe { prealloc_scope_owns(scope, owned.values[0].dependencies.data) }
		assert !unsafe { prealloc_scope_owns(scope, owned.values[0].dependencies[0].str) }
		assert !unsafe { prealloc_scope_owns(scope, owned.error_message.str) }
		unsafe { prealloc_scope_free_after(scope) }
		assert owned.used_field_defaults
		assert owned.values[0].key == 'answer'
		assert owned.values[0].c_name == 'main__answer'
		assert owned.values[0].module_name == 'main'
		assert owned.values[0].value == 'base + 1'
		assert owned.values[0].dependencies == ['base']
		assert owned.values[0].is_runtime
		assert owned.constant_types.value('answer') == 'int'
		assert owned.fixed_array_types.value('FixedArray_2_int') == 'FixedArray_2_int'
		assert owned.composite_types['Array_int']
		assert owned.error_message == 'initializer error'
	}
}

fn test_fastc_file_output_survives_parser_scope_release() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		mut fixed := flat.TypeTextBindings{}
		fixed.set('FixedArray_int_3'.clone(), '[3]int'.clone())
		output := FastcFileGenOutput{
			prototypes:        'void example(void);'.clone()
			body:              'void example(void) {}'.clone()
			directive_lines:   [FastcCDirectiveLine{ start: 1, end: 4, kind: 2 }]
			function_ids:      [3]
			function_spans:    [0, 22]
			proto_spans:       [0, 19]
			ref_starts:        [0, 1]
			refs:              [7]
			root_refs:         [8]
			has_main_entry:    true
			fixed_array_types: fixed
			composite_types:   {
				'Array_example'.clone(): true
			}
			spawn_typedefs:    {
				'thread_example'.clone(): 'typedef void *thread_example;'.clone()
			}
			spawn_helpers:     {
				'spawn_example'.clone(): 'void spawn_example(void) {}'.clone()
			}
			mono_definitions:  {
				'generic_int'.clone(): 'int generic_int(void) { return 3; }'.clone()
			}
			c_flags:           ['-lnative'.clone()]
			failed:            true
			error_message:     'example error'.clone()
		}
		unsafe { prealloc_scope_leave(scope) }
		published := fastc_clone_file_gen_output(&output)
		assert !unsafe { prealloc_scope_owns(scope, published.prototypes.str) }
		assert !unsafe { prealloc_scope_owns(scope, published.body.str) }
		assert !unsafe { prealloc_scope_owns(scope, published.error_message.str) }
		assert !unsafe { prealloc_scope_owns(scope, published.directive_lines.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.function_ids.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.function_spans.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.proto_spans.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.ref_starts.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.refs.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.root_refs.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.fixed_array_types.texts) }
		for key, _ in published.fixed_array_types.ids {
			assert !unsafe { prealloc_scope_owns(scope, key.str) }
		}
		for key, _ in published.composite_types {
			assert !unsafe { prealloc_scope_owns(scope, key.str) }
		}
		for key, value in published.spawn_typedefs {
			assert !unsafe { prealloc_scope_owns(scope, key.str) }
			assert !unsafe { prealloc_scope_owns(scope, value.str) }
		}
		for key, value in published.spawn_helpers {
			assert !unsafe { prealloc_scope_owns(scope, key.str) }
			assert !unsafe { prealloc_scope_owns(scope, value.str) }
		}
		for key, value in published.mono_definitions {
			assert !unsafe { prealloc_scope_owns(scope, key.str) }
			assert !unsafe { prealloc_scope_owns(scope, value.str) }
		}
		assert !unsafe { prealloc_scope_owns(scope, published.c_flags.data) }
		assert !unsafe { prealloc_scope_owns(scope, published.c_flags[0].str) }
		// The next file may overwrite every previous parser allocation.
		assert unsafe { prealloc_scope_reenter(scope, fastc_file_arena_keep_bytes) }
		reused := []u8{len: 4096, init: 0xa5}
		assert reused[0] == 0xa5
		unsafe { prealloc_scope_leave(scope) }
		unsafe { prealloc_scope_free_after(scope) }
		assert published.prototypes == 'void example(void);'
		assert published.body == 'void example(void) {}'
		assert published.directive_lines[0] == FastcCDirectiveLine{ start: 1, end: 4, kind: 2 }
		assert published.function_ids == [3]
		assert published.function_spans == [0, 22]
		assert published.proto_spans == [0, 19]
		assert published.ref_starts == [0, 1]
		assert published.refs == [7]
		assert published.root_refs == [8]
		assert published.has_main_entry
		assert published.fixed_array_types.value('FixedArray_int_3') == '[3]int'
		assert published.composite_types['Array_example']
		assert published.spawn_typedefs['thread_example'] == 'typedef void *thread_example;'
		assert published.spawn_helpers['spawn_example'] == 'void spawn_example(void) {}'
		assert published.mono_definitions['generic_int'] == 'int generic_int(void) { return 3; }'
		assert published.c_flags == ['-lnative']
		assert published.failed
		assert published.error_message == 'example error'
	}
}
