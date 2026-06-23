// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module insel

import v2.mir
import v2.pref

// select performs target-specific instruction selection on MIR.
// The current native backends consume MIR opcodes directly, so there is no
// target-specific rewrite to apply yet. Keep the API as the insertion point for
// a future real instruction selector.
pub fn select(mut m mir.Module, arch pref.Arch) {
	_ = m
	_ = arch
}
