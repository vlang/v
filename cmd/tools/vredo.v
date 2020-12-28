module main

// Implements `v redo file.v` , `v redo run file.v` etc.
// With this command, V will collect all .v files that are needed for the compilation,
// then it will enter an infinite loop, monitoring them for changes.
//
// When a change is detected, it will stop the current process, if it is still running,
// then rerun/recompile/etc.
//
// In effect, this makes it easy to have an editor session and a separate terminal,
// running just `v redo file.v`, and you will see your changes right after you save your
// .v file in your editor.

import os
import os.cmdline
import time

struct Context {
mut:
	opts []string
	redo_options []string
	rest []string
	is_debug bool
}

fn main() {
	mut context := Context{}
	context.opts = os.args[1..]
	context.redo_options = cmdline.options_before(context.opts, ['redo'])
	context.rest = cmdline.options_after(context.opts, ['redo'])
	context.is_debug = '-debug' in os.getenv('VREDO').split(' ')
	if context.is_debug {
		println('context: $context')
	}
	time.sleep_ms(100000)
}
