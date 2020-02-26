// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module flag

import (
	v.pref
)

type void_cb fn(string, &Instance, voidptr)

// This file contains all instance of usage in cmd/v. Should be replaced by generics.
pub fn parse_pref(args []string, callback fn(string, &Instance, &pref.Preferences), obj &pref.Preferences) ?[]string {
	mut p := Instance {
		args: args
		current_pos: -1
	}
	casted_callback := *(&void_cb(&callback))
	tmp := p.parse_impl(args, voidptr(obj), casted_callback) or {
		return error(err)
	}
	return tmp
}
