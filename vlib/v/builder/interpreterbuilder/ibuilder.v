module interpreterbuilder

import v.eval
import v.pref
import v.util
import v.builder

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('interpret', prefs, interpret_v)
}

pub fn interpret_v(mut b builder.Builder) {
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	b.front_and_middle_stages(files) or { return }

	util.timing_start('INTERPRET')
	mut e := eval.new_eval(b.table, b.pref)
	e.eval(mut b.parsed_files)
	util.timing_measure('INTERPRET')
}
