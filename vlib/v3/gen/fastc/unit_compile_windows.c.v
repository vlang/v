module fastc

import os
import v3.cmdexec

struct FastcUnitCompile {
mut:
	process &os.Process = unsafe { nil }
	object  string
}

// fastc_prestart_c_units is unavailable on Windows, whose process wrapper has
// no stdin-pipe prestart path.
pub fn fastc_prestart_c_units(_ string, _ []string, _ string, _ int) FastcPrestartedCUnits {
	return FastcPrestartedCUnits{}
}

// fastc_discard_prestarted_c_units is a no-op on Windows.
pub fn fastc_discard_prestarted_c_units(mut _ FastcPrestartedCUnits) {}

// fastc_begin_feed_prestarted_c_units is unavailable on Windows.
pub fn fastc_begin_feed_prestarted_c_units(mut _ FastcPrestartedCUnits, mut _ FastcRenderingCUnits) !FastcFeedingCUnits {
	return error('prestarted FastC units are unavailable on Windows')
}

// fastc_begin_render_prestarted_c_units is unavailable on Windows.
pub fn fastc_begin_render_prestarted_c_units(mut _ FastcPrestartedCUnits, _ string, _ []string, _ FastcUnitLayout, _ int) !FastcFeedingCUnits {
	return error('prestarted FastC units are unavailable on Windows')
}

// fastc_finish_prestarted_c_units is unavailable on Windows.
pub fn fastc_finish_prestarted_c_units(mut _ FastcPrestartedCUnits, mut _ FastcFeedingCUnits, _ FastcPreparedUnits, mut _ FastcPreparedLink, _ bool) ![]string {
	return error('prestarted FastC units are unavailable on Windows')
}

// fastc_compile_prestarted_rendering_c_units is unavailable on Windows.
pub fn fastc_compile_prestarted_rendering_c_units(mut _ FastcPrestartedCUnits, mut _ FastcRenderingCUnits, _ FastcPreparedUnits, mut _ FastcPreparedLink, _ bool) ![]string {
	return error('prestarted FastC units are unavailable on Windows')
}

// fastc_compile_c_units compiles the translation units to objects with
// concurrent TinyCC processes and returns the object paths, or the output of
// the first compile that failed.
pub fn fastc_compile_c_units(tcc string, base_args []string, unit_paths []string, prepared FastcPreparedUnits) ![]string {
	mut compiles := []FastcUnitCompile{cap: unit_paths.len}
	for i in fastc_unit_compile_order(unit_paths, prepared) {
		unit_path := unit_paths[i]
		entry := prepared.entries[i]
		object := entry.object
		mut args := base_args.clone()
		args << ['-c', unit_path, '-o', object]
		mut process := os.new_process(tcc)
		process.set_args(args)
		process.set_redirect_stdio_merged()
		process.run()
		compiles << FastcUnitCompile{
			process: process
			object: object
		}
	}
	mut failure := ''
	for mut compile in compiles {
		compile.process.wait()
		output := compile.process.stdout_slurp()
		code := compile.process.code
		compile.process.close()
		if code != 0 && failure == '' {
			failure = if output.len > 0 { output } else { 'tcc failed on ${compile.object}' }
		}
	}
	if failure != '' {
		return error(failure)
	}
	for entry in prepared.entries {
		fastc_publish_unit_cache(entry)
	}
	return prepared.objects
}

// fastc_compile_c_unit_texts keeps the cross-platform API available; Windows
// currently uses temporary files because its process wrapper has no stdin pipe.
pub fn fastc_compile_c_unit_texts(tcc string, base_args []string, unit_paths []string, sources []string, prepared FastcPreparedUnits) ![]string {
	if unit_paths.len != sources.len {
		return error('invalid streamed FastC unit layout')
	}
	for i, source in sources {
		os.write_file(unit_paths[i], source)!
	}
	return fastc_compile_c_units(tcc, base_args, unit_paths, prepared)
}

// fastc_run_command runs the program with the argument vector and returns
// its exit code and merged output.
pub fn fastc_run_command(program string, args []string) os.Result {
	return cmdexec.run(program, args)
}
