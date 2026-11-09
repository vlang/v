module fastc

import os
import v.cmdexec

fn test_conditional_returns_in_struct_result_or_blocks_match_c() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_or_conditional_return_struct_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Box { number f64 }
fn make_box(fail bool) !Box {
    if fail { return error("requested") }
    return Box{number: 9.5}
}
fn forward(fail bool, replace bool) !Box {
    box := make_box(fail) or {
        if replace { return error("replacement") } else { return err }
    }
    return box
}
fn fallback(fail bool, propagate bool) !Box {
    box := make_box(fail) or {
        if propagate { return err } else { Box{number: 4.25} }
    }
    return box
}
fn main() {
    success := forward(false, true) or { panic(err) }
    assert success.number == 9.5
    if _ := forward(true, true) { panic("expected replacement") } else {
        assert err.msg() == "replacement"
    }
    if _ := forward(true, false) { panic("expected propagation") } else {
        assert err.msg() == "requested"
    }
    alternate := fallback(true, false) or { panic(err) }
    assert alternate.number == 4.25
    if _ := fallback(true, true) { panic("expected fallback propagation") } else {
        assert err.msg() == "requested"
    }
    println("conditional struct returns work")
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'conditional struct returns work', run.output
	}
}
