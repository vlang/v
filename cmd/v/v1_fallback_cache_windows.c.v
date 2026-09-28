// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import crypto.rand
import os

fn v1_fallback_private_windows_temp_cache_parent(temp_root string) !string {
	for _ in 0 .. 64 {
		random_suffix := rand.bytes(16) or {
			return error('could not generate a private V1 fallback cache name: ${err}')
		}
		candidate := os.join_path(temp_root, 'v1-fallback-cache-${random_suffix.hex()}')
		os.mkdir(candidate, mode: 0o700) or {
			if os.exists(candidate) {
				continue
			}
			return error('could not create a private V1 fallback cache at `${candidate}`: ${err}')
		}
		return candidate
	}
	return error('could not reserve a unique private V1 fallback cache in `${temp_root}`')
}
