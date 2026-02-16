// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

fn map_get_type_id(m map[string]TypeID, key string) ?TypeID {
	for k, v in m {
		if k == key {
			return v
		}
	}
	return none
}

fn map_get_value_id(m map[string]ValueID, key string) ?ValueID {
	for k, v in m {
		if k == key {
			return v
		}
	}
	return none
}
