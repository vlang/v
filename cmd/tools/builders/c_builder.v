module main

import v.builder.cbuilder

// TODO: change bootstrapping to use the C code generated from
// `VEXE=v cmd/tools/builders/c_builder -os cross -o c.c cmd/tools/builders/c_builder.v`
// See also `cmd/v/v.v`

fn main() {
	cbuilder.start()
}
