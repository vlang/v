// vtest build: !docker-ubuntu-musl
module gg

import os
import v.cmdexec

fn test_context_end_does_not_heap_copy_a_frame_local_pass() {
	root := os.join_path(os.vtmp_dir(), 'gg_retained_pass_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'import gg
fn main() {
	ctx := &gg.Context{}
	ctx.end()
}
') or { panic(err) }
	compiled := cmdexec.run(@VEXE, ['-b', 'c', '-nocache', '-o', output, source])
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(output) or { panic(err) }
	body := generated.all_after_last('void gg__Context__end(').all_after('{').all_before('\n}')
	assert body.contains('gfx__begin_pass'), body
	assert !body.contains('memdup'), body
}
