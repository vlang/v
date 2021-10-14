// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Please see https://github.com/floooh/sokol-tools/blob/master/docs/sokol-shdc.md#feature-overview
// For a more in-depth overview of the tool in use.
import os
import flag
import net.http

const (
	tool_name        = os.file_name(os.executable())
	tool_version     = '0.0.1'
	tool_description = 'Compile shaders in sokol format to C headers for use with sokol based apps'
	cache_dir        = os.join_path(os.cache_dir(), 'v', tool_name)
	runtime_os       = os.user_os()
)

const (
	supported_hosts  = ['linux', 'macos', 'windows']
	supported_slangs = [
		'glsl330', // desktop GL
		'glsl100', // GLES2 / WebGL
		'glsl300es', // GLES3 / WebGL2
		'hlsl4', // D3D11
		'hlsl5', // D3D11
		'metal_macos', // Metal on macOS
		'metal_ios', // Metal on iOS device
		'metal_sim', // Metal on iOS simulator
		'wgpu', // WebGPU
	]
	default_slangs   = [
		'glsl330', // desktop GL
		'glsl100', // GLES2 / WebGL
		'glsl300es', // GLES3 / WebGL2
		'hlsl5', // D3D11
		'metal_macos', // Metal on macOS
		'metal_ios', // Metal on iOS device
		'metal_sim', // Metal on iOS simulator
		'wgpu', // WebGPU
	]

	shdc_version     = '33d2e4cc'
	shdc_urls        = {
		'windows': 'https://github.com/floooh/sokol-tools-bin/raw/33d2e4cc26088c6c28eaef5467990f8940d15aab/bin/win32/sokol-shdc.exe'
		'macos':   'https://github.com/floooh/sokol-tools-bin/raw/33d2e4cc26088c6c28eaef5467990f8940d15aab/bin/osx/sokol-shdc'
		'linux':   'https://github.com/floooh/sokol-tools-bin/raw/33d2e4cc26088c6c28eaef5467990f8940d15aab/bin/linux/sokol-shdc'
	}
	shdc_version_file = os.join_path(cache_dir, 'sokol-shdc.version')
	shdc              = shdc_exe()
)

struct Options {
	show_help    bool
	verbose      bool
	force_update bool
	slangs       []string
	output       string
}

fn main() {
	if os.args.len == 1 {
		println('Usage: $tool_name PATH \n$tool_description\n$tool_name -h for more help...')
		exit(1)
	}
	mut fp := flag.new_flag_parser(os.args[1..])
	fp.application(tool_name)
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('PATH [PATH]...')
	fp.skip_executable()
	// Collect tool options
	opt := Options{
		show_help: fp.bool('help', `h`, false, 'Show this help text.')
		force_update: fp.bool('force-update', `u`, false, 'Force update of the sokol-shdc tool.')
		verbose: fp.bool('verbose', `v`, false, 'Be verbose about the tools progress.')
		slangs: fp.string_multi('slang', `l`, 'Shader dialects to generate code for. Default is all. Available: $supported_slangs')
		output: fp.string('output', `o`, '', 'Place output here. <name>.h, path or path/<name>.h')
	}
	if opt.show_help {
		println(fp.usage())
		exit(0)
	}

	ensure_external_tools(opt) or { panic(err) }

	input_paths := fp.finalize() or { panic(err) }

	for path in input_paths {
		if os.exists(path) {
			compile_shaders(opt, path) or { panic(err) }
		}
	}
}

fn validate_shader_file(path string) ? {
	shader_program := os.read_lines(path) or {
		return error('shader program at "$path" could not be opened for reading')
	}
	mut has_program_directive := false
	for line in shader_program {
		if line.contains('@program ') {
			has_program_directive = true
			break
		}
	}
	if !has_program_directive {
		return error('shader program at "$path" is missing a "@program" directive.')
	}
}

fn compile_shaders(opt Options, input_path string) ? {
	mut path := input_path.trim_right('/')
	path = path.trim_right('/')
	if os.is_file(path) {
		path = os.dir(path)
	}

	mut shader_files := []string{}
	collect_fn := fn (path string, mut list []string) {
		if os.file_ext(path) == '.glsl' {
			list << os.real_path(path)
		}
	}
	collect(path, mut shader_files, collect_fn)

	if shader_files.len == 0 {
		if opt.verbose {
			eprintln('$tool_name found no shader files to compile for "$path"')
		}
		return
	}
	mut flat_shader_files := ''
	is_multiple := shader_files.len > 1
	for shader_file in shader_files {
		// It could be the user has WIP shader files lying around not used,
		// so we just report that there's something wrong
		validate_shader_file(shader_file) or {
			eprintln(err)
			continue
		}
		flat_shader_files += '--input "$shader_file" '
	}

	mut out_file := os.file_name(shader_files[0]).all_before('.') + '.h'

	// TODO better heuristics/convention for the output name
	if is_multiple {
		out_file = os.file_name(path).all_before('.') + '.h'
	}

	// User provided output form
	if opt.output != '' {
		if os.is_dir(opt.output) {
			out_file = os.join_path(opt.output, out_file)
		} else if opt.output.contains(os.path_separator) {
			out_file = opt.output
		} else {
			out_file = os.join_path(path, opt.output)
		}
	} else {
		out_file = os.join_path(path, out_file)
	}

	mut slangs := []string{}
	if opt.slangs.len == 0 {
		unsafe {
			slangs = default_slangs
		}
	}

	flat_shader_files = flat_shader_files.trim_right(' ')
	if opt.verbose {
		eprintln('$tool_name generating shader code for $slangs in header "${os.file_name(out_file)}" in "$path" from $shader_files')
	}

	cmd := '$shdc $flat_shader_files --output $out_file --slang ' + slangs.join(':')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('$tool_name failed generating shader includes:\n        $res.output\n        $cmd')
	}
}

fn collect(path string, mut list []string, collect_fn fn (string, mut []string)) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or { return }
	for file in files {
		p := os.join_path(path, file)
		if os.is_dir(p) && !os.is_link(p) {
			collect(p, mut list, collect_fn)
		} else if os.exists(p) {
			collect_fn(p, mut list)
		}
	}
	return
}

fn ensure_external_tools(opt Options) ? {
	if !os.exists(cache_dir) {
		os.mkdir_all(cache_dir) ?
	}
	if opt.force_update {
		download_shdc(opt) ?
		return
	}

	is_shdc_available := os.is_file(shdc)
	is_shdc_executable := os.is_executable(shdc)
	if is_shdc_available && is_shdc_executable {
		if opt.verbose {
			version := os.read_file(shdc_version_file) or { 'unknown' }
			eprintln('$tool_name using sokol-shdc version $version at "$shdc"')
		}
		return
	}

	download_shdc(opt) ?
}

fn shdc_exe() string {
	if runtime_os == 'windows' {
		return os.join_path(cache_dir, 'sokol-shdc.exe')
	}
	return os.join_path(cache_dir, 'sokol-shdc')
}

fn download_shdc(opt Options) ? {
	// We want to use the same, runtime, OS type as this tool is invoked on.
	download_url := shdc_urls[runtime_os] or { '' }
	if download_url == '' {
		return error('$tool_name failed to download an external dependency "sokol-shdc" for ${runtime_os}.\nThe supported host platforms for shader compilation is $supported_hosts')
	}
	update_to_shdc_version := os.read_file(shdc_version_file) or { shdc_version }
	file := shdc_exe()
	if opt.verbose {
		if shdc_version != update_to_shdc_version && os.exists(file) {
			eprintln('$tool_name updating sokol-shdc to version $update_to_shdc_version ...')
		} else {
			eprintln('$tool_name installing sokol-shdc version $update_to_shdc_version ...')
		}
	}
	http.download_file(download_url, file) or {
		return error('$tool_name failed to download sokol-shdc needed for shader compiling: $err')
	}
	// Make it executable
	os.chmod(file, 0o775) ?
	// Update internal version file
	os.write_file(shdc_version_file, update_to_shdc_version) ?
}
