import os

const sife_vexe = @VEXE
const sife_tests_dir = os.dir(@FILE)
const sife_v3_dir = os.dir(sife_tests_dir)
const sife_vlib_dir = os.dir(sife_v3_dir)
const sife_v3_src = os.join_path(sife_v3_dir, 'v3.v')

fn sife_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_struct_init_embed_fixed_array_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${sife_vexe} -gc none -path "${sife_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${sife_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn sife_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn sife_run_project(v3_bin string, name string, files map[string]string) string {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_${name}_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		sife_write_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A struct that both embeds a type and has a fixed-array field routes its initializer through
// the fixed-array helper (a compound literal plus a memcpy tail, because C compound literals
// cannot assign array members). The embedded-field key (`Ctx: c`) is a short name whose C field
// name comes from the embed type (`base__Ctx`), not the source key, and it is not in the
// helper's `allowed_fields` set. Without mirroring the embedded-key branch in the fixed-array
// helper the embed initializer is dropped (or emitted under a non-existent `.Ctx` designator),
// leaving the embedded struct at its defaults. Here the embed carries `code: 9`, so a dropped
// initializer would surface as `code=0`.
fn test_struct_init_embed_plus_fixed_array_sets_both() {
	v3_bin := sife_build_v3()
	out := sife_run_project(v3_bin, 'struct_init_embed_fixed_array', {
		'base/base.v': 'module base\n\npub struct Ctx {\npub mut:\n\tcode int\n}\n'
		'main.v':      "module main\n\nimport base\n\nstruct Outer {\n\tbase.Ctx\nmut:\n\tids [2]int\n}\n\nfn main() {\n\tc := base.Ctx{\n\t\tcode: 9\n\t}\n\to := Outer{\n\t\tCtx:  c\n\t\tids:  [4, 5]!\n\t}\n\tprintln('code=\${o.code} ids=\${o.ids[0]},\${o.ids[1]}')\n}\n"
	})
	assert out == 'code=9 ids=4,5'
}
