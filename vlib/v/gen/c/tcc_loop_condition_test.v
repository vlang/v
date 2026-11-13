module c

import os
import v.cmdexec

fn test_tcc_loop_conditions_preserve_short_circuit_break_continue_and_post_steps() ! {
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc loop conditions ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct LoopState {\nmut:\n calls int\n}\nfn step(mut state LoopState) ?int { state.calls++; if state.calls == 3 { return none }; return state.calls }\nfn accept(n int) bool { return n < 5 }\nfn main() {\n mut state := LoopState{}\n mut sum := 0\n for state.calls < 5 && accept(step(mut state) or { break }) { sum += state.calls }\n assert sum == 3 && state.calls == 3\n state = LoopState{}; sum = 0\n for state.calls < 5 && accept(step(mut state) or { continue }) { sum += state.calls }\n assert sum == 7 && state.calls == 5\n state = LoopState{}; sum = 0\n for i := 0; i < 6 && accept(step(mut state) or { continue }); i++ { sum += i }\n assert sum == 4 && state.calls == 5\n state = LoopState{}\n for state.calls < 0 && accept(step(mut state) or { break }) { assert false }\n assert state.calls == 0\n}\n')!
	build := cmdexec.run(@VEXE, ['-cc', 'tcc', '-gc', 'none', '-nocache', '-no-retry-compilation',
		'-o', os.join_path(root, 'program'), 'run', source])
	assert build.exit_code == 0, build.output
}
