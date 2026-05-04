// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.pref as vpref
import v2.types

fn new_test_transformer_for_in() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
}

fn test_for_in_value_type_treats_string_struct_as_u8() {
	mut t := new_test_transformer_for_in()

	got := t.for_in_value_type(types.Type(types.Struct{
		name: 'string'
	}))

	assert got.name() == 'u8'
}

fn test_for_in_value_type_treats_string_alias_as_u8() {
	mut t := new_test_transformer_for_in()

	got := t.for_in_value_type(types.Type(types.Alias{
		name:      'my_string'
		base_type: types.Type(types.Struct{
			name: 'string'
		})
	}))

	assert got.name() == 'u8'
}
