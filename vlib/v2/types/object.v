// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import v2.token

struct ObjectCommon {
	parent &Scope = unsafe { nil }
	pos    token.Pos
	mod    &Module
	name   string
mut:
	typ Type
	// order_    u32
	// color_    color
}

struct Const {
	ObjectCommon
}

struct Global {
	name string
	typ  Type
}

struct Fn {
	name string
	// typ  FnType // signature
	typ Type // signature
}
