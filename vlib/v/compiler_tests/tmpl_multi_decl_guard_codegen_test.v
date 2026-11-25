import os
import v.tempname

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) with a
// MULTI-declaration guard — `@if a, b := pair() { @{a}@{b} }` — must declare every LHS name for the
// branch, not just the first. The flat `decl_assign` interleaves LHS and RHS (`[a, rhs, b]`), so
// naively collecting every child after index 0 walks the later LHS `b` as a free outer identifier;
// the closure would then capture a non-existent outer `b`. All LHS slots must be recognized as
// branch-local.
fn test_template_multi_decl_guard_declares_all_lhs_names() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_multi_decl_guard_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'mdg' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 't.html'), '@if a, b := pair()\n@{a}@{b}\n@end\n') or {
		panic(err)
	}
	source := "module main\n\nfn pair() ?(string, string) {\n\treturn 'A', 'B'\n}\n\nfn build() string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build())\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// Both guard bindings printed inside the branch; neither was captured as an outer variable.
	assert run.output.trim_space() == '[AB]', run.output
}
