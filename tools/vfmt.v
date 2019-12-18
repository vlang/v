// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	os
	compiler
)

fn main() {
	toolexe := os.executable()
	compiler.set_vroot_folder( os.dir(os.dir(toolexe)) )
	args := compiler.env_vflags_and_os_args()
	println('args: $args')  
	compiler.vfmt(args)
}
