// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import v2.pref
import v2.token

fn test_for_in_value_type_treats_string_struct_as_u8() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	checker := Checker.new(prefs, file_set, env)

	got := checker.for_in_value_type(Type(Struct{
		name: 'string'
	}))

	assert got.name() == 'u8'
}

fn test_for_in_value_type_treats_string_alias_as_u8() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	checker := Checker.new(prefs, file_set, env)

	got := checker.for_in_value_type(Type(Alias{
		name:      'my_string'
		base_type: Type(Struct{
			name: 'string'
		})
	}))

	assert got.name() == 'u8'
}
