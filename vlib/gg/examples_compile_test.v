// vtest build: !musl? && !sanitized_job?
import os
import runtime

// gg_examples_child_flags returns extra flags needed for compiling the example
// programs on the current platform.
fn gg_examples_child_flags() []string {
	$if linux_wayland_session ? {
		$if sokol_wayland ? {
			return ['-d', 'sokol_wayland']
		}
	}
	return []
}

// gg_examples_to_compile lists all gg based examples shipped in the repo.
// Every one of them has to keep compiling, after the gg module switched from
// sokol.sgl to its own pure-V renderer (vlib/gg/render/), because examples
// that still mix gg and sgl calls either stop typechecking (render.Pipeline
// is not sgl.Pipeline), or silently record nothing at all.
fn gg_examples_to_compile() []string {
	vroot := os.dir(@VEXE)
	mut samples := os.walk_ext(os.join_path(vroot, 'examples/gg'), '.v')
	samples << [
		os.join_path(vroot, 'examples/breakout/breakout.v'),
		os.join_path(vroot, 'examples/ttf_font/draw_static_text.v'),
		os.join_path(vroot, 'examples/ttf_font/example_ttf.v'),
		// the image viewer is a module directory, it compiles as a whole
		os.join_path(vroot, 'examples/viewer'),
	]
	samples.sort()
	return samples
}

struct GGExampleCompileResult {
	source string
	output string
	exists bool
}

fn gg_example_compile_worker(source string, vexe string, child_flags []string, cache_dir string) GGExampleCompileResult {
	mut out_exe := os.join_path(cache_dir, os.file_name(source).replace('.v', ''))
	$if windows {
		out_exe += '.exe'
	}
	cmd := [vexe, '-o', out_exe, ...child_flags, source]
	res := os.exec(cmd)
	if res.exit_code != 0 {
		return GGExampleCompileResult{ source: source, output: res.output, exists: false }
	}
	return GGExampleCompileResult{ source: source, output: '', exists: os.exists(out_exe) }
}

fn gg_examples_compile_all_samples(vexe string, child_flags []string, cache_dir string, samples []string) []GGExampleCompileResult {
	mut sources := chan string{cap: samples.len}
	results := chan GGExampleCompileResult{cap: samples.len}
	nr_workers := runtime.nr_jobs()
	for _ in 0 .. nr_workers {
		spawn gg_example_compile_loop(sources, results, vexe, child_flags, cache_dir)
	}
	for s in samples {
		sources <- s
	}
	sources.close()
	mut all := []GGExampleCompileResult{cap: samples.len}
	for _ in 0 .. samples.len {
		all << <-results
	}
	results.close()
	all.sort(a.source < b.source)
	return all
}

fn gg_example_compile_loop(source_ch chan string, results chan GGExampleCompileResult, vexe string,
	child_flags []string, cache_dir string) {
	for {
		source := <-source_ch or { break }
		results <- gg_example_compile_worker(source, vexe, child_flags, cache_dir)
	}
}

fn test_all_gg_examples_can_be_compiled() {
	vexe := @VEXE
	vroot := os.dir(vexe)
	cache_dir := os.join_path(os.vtmp_dir(), 'gg_examples_vcache_${os.getpid()}')
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(cache_dir) or { panic(err) }
	old_vcache := os.getenv_opt('VCACHE')
	os.setenv('VCACHE', cache_dir, true)
	defer {
		if vcache := old_vcache {
			os.setenv('VCACHE', vcache, true)
		} else {
			os.unsetenv('VCACHE')
		}
		os.rmdir_all(cache_dir) or {}
	}
	child_flags := gg_examples_child_flags()
	samples := gg_examples_to_compile()
	mut fails := []string{}
	for res in gg_examples_compile_all_samples(vexe, child_flags, cache_dir, samples) {
		if !res.exists {
			eprintln('>>> FAIL ${res.source}')
			eprintln(res.output)
			fails << res.source
			continue
		}
		println('OK ${res.source}')
	}
	if fails.len > 0 {
		eprintln('> Failed summary:')
		for f in fails {
			eprintln('   failed sample: ${f}')
		}
		assert false
	}
}
