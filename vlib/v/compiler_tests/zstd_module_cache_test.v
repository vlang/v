import os

fn test_cached_module_embedded_resources_refresh_and_reject_deletion() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'module_cache_embed_${os.getpid()}')
	name := 'cache_embed_${os.getpid()}'
	module_root := os.join_path(@VEXEROOT, 'vlib', name)
	os.mkdir_all(root)!
	os.mkdir_all(module_root)!
	defer {
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	resource := os.join_path(module_root, 'payload.txt')
	os.write_file(resource, 'first')!
	os.write_file(os.join_path(module_root, 'source.v'), 'module ${name}\npub fn value() string { return \$embed_file("payload.txt").to_string() }\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import ${name}\nfn main() { println(${name}.value()) }\n')!
	binary := os.join_path(root, 'program')
	args := ['env', 'V3CACHE=${os.join_path(root, 'cache')}', @VEXE, '-new-compiler', '-cc', 'cc',
		'-no-retry-compilation', '-show-timings', '-o', binary, source]
	cold := os.exec(args)
	assert cold.exit_code == 0, cold.output
	warm := os.exec(args)
	assert warm.exit_code == 0 && warm.output.contains('monomorphize (cached)'), warm.output
	first := os.exec([binary])
	assert first.exit_code == 0 && first.output.trim_space() == 'first', first.output
	os.write_file(resource, 'other')!
	changed := os.exec(args)
	assert changed.exit_code == 0, changed.output
	other := os.exec([binary])
	assert other.exit_code == 0 && other.output.trim_space() == 'other', other.output
	os.rm(resource)!
	missing := os.exec(args)
	assert missing.exit_code != 0, missing.output
}

fn test_shipped_zstd_and_veb_use_module_cache() {
	$if windows {
		return
	} // This regression uses the Unix system C compiler cache path.
	root := os.join_path(os.vtmp_dir(), 'zstd_module_cache_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	sources := {
		'zstd': "import compress.zstd
fn main() {
 data := 'cached native source'.bytes()
 encoded := zstd.compress(data)!
 assert zstd.decompress(encoded)! == data
 println('zstd ok')
}"
		'veb':  'import veb
struct App { veb.Middleware[Context] }
struct Context { veb.Context }
pub fn (mut app App) index(mut ctx Context) veb.Result { return ctx.text("hello") }
fn main() {
 mut app := &App{}
 veb.run[App, Context](mut app, 18099)
}'
	}
	for name, source_text in sources {
		src := os.join_path(root, '${name}.v')
		binary := os.join_path(root, name + $if windows { '.exe' } $else { '' })
		cache := os.join_path(root, '${name}_cache')
		os.write_file(src, source_text)!
		args := ['env', 'V3CACHE=${cache}', 'V3_CACHE_TRACE=1', @VEXE, '-new-compiler', '-cc',
			'cc', '-no-retry-compilation', '-show-timings', '-o', binary, src]
		cold := os.exec(args)
		assert cold.exit_code == 0, cold.output
		assert !cold.output.contains('external C inputs cannot be assigned'), cold.output
		warm := os.exec(args)
		assert warm.exit_code == 0, warm.output
		assert warm.output.contains('monomorphize (cached)'), warm.output
		assert !warm.output.contains('external C inputs cannot be assigned'), warm.output
		if name == 'zstd' {
			run := os.exec([binary])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == 'zstd ok', run.output
		}
	}
}
