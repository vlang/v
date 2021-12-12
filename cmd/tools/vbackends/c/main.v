module main

import v.pref
import v.util
import v.builder
import v.builder.cbackend

// TODO: change bootstrapping to use the C code generated from
// `VEXE=v cmd/tools/vbackends/c/c -os cross -o c.c cmd/tools/vbackends/c/main.v`
// See also `cmd/v/v.v`

fn main() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, cbackend.compile_c)
}
