// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

// replace_file_atomically moves `source` onto `destination`, replacing it if it is already
// there. POSIX `rename(2)` is defined to do exactly that, atomically, and a process that is
// currently executing the replaced binary keeps running from its own open image.
fn replace_file_atomically(source string, destination string) bool {
	os.rename(source, destination) or { return false }
	return true
}
