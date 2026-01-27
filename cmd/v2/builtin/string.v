// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Builtin string struct for v2 SSA backend testing.
// This must match the definition in vlib/v2/types/universe.v

pub struct string {
	str    &u8
	len    int
	is_lit int
}
