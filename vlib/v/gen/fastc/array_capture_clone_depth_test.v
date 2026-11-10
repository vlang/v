module fastc

import os
import v.cmdexec

// A captured `[]T` is cloned one level deep only when its elements are arrays.
// Elements that merely have the size of a string or an array keep their
// pointer fields; they must not be reinterpreted and deep-copied.
fn test_closure_array_captures_clone_only_nested_arrays() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_array_capture_clone_depth_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
@[heap]
struct Target {
mut:
	active     bool
	generation u64
}
// Same layout size as a string: a pointer followed by two ints.
struct StringSized {
	target     &Target
	generation int
	marker     int
}
// Same layout size as an array: a pointer followed by five ints.
struct ArraySized {
	target &Target
	a      int
	b      int
	c      int
	d      int
	e      int
}
fn main() {
    // Ensure these elements trigger both size-based paths in the old clone_to_depth(1).
    assert sizeof(StringSized) == sizeof(string)
    assert sizeof(ArraySized) == sizeof(array)
    mut target := &Target{active: true, generation: 48}
    // An old string-clone interpretation uses generation as a byte count: 8 is in bounds.
    items := [StringSized{target: target, generation: 8, marker: 99}]
    wide := [ArraySized{target: target}]
    check := fn [items, wide, target] () bool {
        return voidptr(items[0].target) == voidptr(target)
            && voidptr(wide[0].target) == voidptr(target)
            && items[0].target.active && items[0].generation == 8 && items[0].marker == 99
    }
    assert voidptr(items[0].target) == voidptr(target)
    assert voidptr(wide[0].target) == voidptr(target)
    target.active = false
    stop := fn [items] () {
        mut t := items[0].target
        t.active = true
    }
    stop()
    assert target.active
    assert check()
    mut matrix := [[1, 2], [3, 4]]
    nested := fn [matrix] () int { return matrix[1][0] }
    matrix[1][0] = 30
    assert nested() == 3
    names := ["a", "b"]
    joined := fn [names] () string { return names.join(",") }
    assert joined() == "a,b"
}
')!
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
