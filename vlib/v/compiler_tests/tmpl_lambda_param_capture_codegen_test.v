import os
import v.tempname

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) whose
// interpolation contains a lambda — `@{items.map(|item| item.name)}` — must NOT capture the lambda
// parameter `item` into the outer closure. `item` is scoped to the lambda body, so capturing it
// would make the generated `fn [items, item] ...` reference a non-existent outer `item` (or the
// wrong shadowed one) and reject otherwise valid template code. Only the real outer local `items`
// is captured.
fn test_template_iife_does_not_capture_lambda_parameters() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_lambda_param_capture_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tlp' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 't.html'), '[@{items.map(|item| item.name).join(",")}]') or {
		panic(err)
	}
	source := "module main\n\nstruct Item {\n\tname string\n}\n\nfn build(items []Item) string {\n\treturn ('<' + \$tmpl('t.html') + '>').replace('\\n', '')\n}\n\nfn main() {\n\titems := [Item{name: 'a'}, Item{name: 'b'}]\n\tprintln(build(items))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The lambda mapped each item to its name; only `items` was captured, `item` stayed local.
	assert run.output.trim_space() == '<[a,b]>', run.output
}
