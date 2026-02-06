// Copyright (c) 2024 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// vshader aids in generating special shader code C headers via sokol-shdc's 'annotated GLSL' format to any
// supported target formats that sokol_gfx supports internally.
//
// vshader bootstraps itself by downloading its own dependencies to a system cache directory on first run.
//
// Please see https://github.com/floooh/sokol-tools/blob/master/docs/sokol-shdc.md#feature-overview
// for a more in-depth overview of the specific tool in use.
//
// The shader language used is, as described on the overview page linked above, an 'annotated GLSL'
// and 'modern GLSL' (v450) shader language format.
import os
import time
import io.util
import flag
import net.http

const shdc_full_hash = '0d91b038780614a867f2c8eecd7d935d76bcaae3'
const tool_version = '0.0.4'
const tool_description = "Compile shaders in sokol's annotated GLSL format to C headers for use with sokol based apps"
const tool_name = os.file_name(os.executable())
const cache_dir = os.join_path(os.cache_dir(), 'v', tool_name)
const runtime_os = os.user_os()
const supported_hosts = ['linux', 'macos', 'windows']
const supported_slangs = [
	'glsl430', // default desktop OpenGL backend (SOKOL_GLCORE)
	'glsl410', // default macOS desktop OpenGL
	'glsl300es', // OpenGLES3 and WebGL2 (SOKOL_GLES3)
	'hlsl4', // Direct3D11 with HLSL4 (SOKOL_D3D11)
	'hlsl5', // Direct3D11 with HLSL5 (SOKOL_D3D11)
	'metal_macos', // Metal on macOS (SOKOL_METAL)
	'metal_ios', // Metal on iOS devices (SOKOL_METAL)
	'metal_sim', // Metal on iOS simulator (SOKOL_METAL)
	'wgsl', // WebGPU (SOKOL_WGPU)
	'reflection',
]
const default_slangs = [
	'glsl410',
	'glsl300es',
	// 'hlsl4', and hlsl5 can't be used at the same time
	'hlsl5',
	'metal_macos',
	'metal_ios',
	'metal_sim',
	'wgsl',
]

const shdc_version = shdc_full_hash[0..8]
const shdc_urls = {
	'windows': 'https://github.com/floooh/sokol-tools-bin/raw/${shdc_full_hash}/bin/win32/sokol-shdc.exe'
	'macos':   'https://github.com/floooh/sokol-tools-bin/raw/${shdc_full_hash}/bin/osx/sokol-shdc'
	'linux':   'https://github.com/floooh/sokol-tools-bin/raw/${shdc_full_hash}/bin/linux/sokol-shdc'
	'osx_a64': 'https://github.com/floooh/sokol-tools-bin/raw/${shdc_full_hash}/bin/osx_arm64/sokol-shdc'
}
const shdc_version_file = os.join_path(cache_dir, 'sokol-shdc.version')
const shdc_exe = os.join_path(cache_dir, 'sokol-shdc.exe')

struct Options {
	show_help    bool
	verbose      bool
	force_update bool
	slangs       []string
}

struct CompileOptions {
	verbose     bool
	slangs      []string
	invoke_path string
}

fn main() {
	if os.args.len == 1 {
		println('Usage: ${tool_name} PATH \n${tool_description}\n${tool_name} -h for more help...')
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
		show_help:    fp.bool('help', `h`, false, 'Show this help text.')
		force_update: fp.bool('force-update', `u`, false, 'Force update of the sokol-shdc tool.')
		verbose:      fp.bool('verbose', `v`, false, 'Be verbose about the tools progress.')
		slangs:       fp.string_multi('slang', `l`, 'Shader dialects to generate code for. Default is all.\n                            Available dialects: ${supported_slangs}')
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

// shader_program_name returns the name of the program from `shader_file`.
// shader_program_name returns a blank string if no @program entry could be found.
fn shader_program_name(shader_file string) string {
	shader_program := os.read_lines(shader_file) or { return '' }
	for line in shader_program {
		if line.contains('@program ') {
			return line.all_after('@program ').all_before(' ')
		}
	}
	return ''
}

// validate_shader_file returns an error if `shader_file` isn't valid.
fn validate_shader_file(shader_file string) ! {
	shader_program := os.read_lines(shader_file) or {
		return error('shader program at "${shader_file}" could not be opened for reading')
	}
	mut has_program_directive := false
	for line in shader_program {
		if line.contains('@program ') {
			has_program_directive = true
			break
		}
	}
	if !has_program_directive {
		return error('shader program at "${shader_file}" is missing a "@program" directive.')
	}
}

// compile_shaders compiles all `*.glsl` files found in `input_path`
// to their C header file representatives.
fn compile_shaders(opt Options, input_path string) ! {
	mut path := os.real_path(input_path)
	path = path.trim_right('/')
	if os.is_file(path) {
		path = os.dir(path)
	}

	mut shader_files := []string{}
	collect(path, mut shader_files)

	if shader_files.len == 0 {
		if opt.verbose {
			eprintln('${tool_name} found no shader files to compile for "${path}"')
		}
		return
	}

	for shader_file in shader_files {
		// It could be the user has WIP shader files lying around not used,
		// so we just report that there's something wrong
		validate_shader_file(shader_file) or {
			eprintln(err)
			continue
		}
		co := CompileOptions{
			verbose:     opt.verbose
			slangs:      opt.slangs
			invoke_path: path
		}
		// Currently sokol-shdc allows for multiple --input flags
		// - but it's only the last entry that's actually compiled/used
		// Given this fact - we can only compile one '.glsl' file to one C '.h' header
		compile_shader(co, shader_file)!
	}
}

// compile_shader compiles `shader_file` to a C header file.
fn compile_shader(opt CompileOptions, shader_file string) ! {
	path := opt.invoke_path
	// The output convention, for now, is to use the name of the .glsl file
	mut out_file := os.file_name(shader_file).all_before_last('.') + '.h'
	out_file = os.join_path(path, out_file)

	mut slangs := opt.slangs.clone()
	if opt.slangs.len == 0 {
		slangs = default_slangs.clone()
	}

	header_name := os.file_name(out_file)
	if opt.verbose {
		eprintln('${tool_name} generating shader code for ${slangs} in header "${header_name}" in "${path}" from ${shader_file}')
	}

	cmd :=
		'${os.quoted_path(shdc_exe)} --input ${os.quoted_path(shader_file)} --output ${os.quoted_path(out_file)} --slang ' +
		os.quoted_path(slangs.join(':'))
	if opt.verbose {
		eprintln('${tool_name} executing:\n${cmd}')
	}
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('${tool_name} failed generating shader includes:\n        ${res.output}\n        ${cmd}')
		exit(1)
	}
	if opt.verbose {
		program_name := shader_program_name(shader_file)
		eprintln('${tool_name} usage example in V:\n\nimport sokol.gfx\n\n#include "${header_name}"\n\nfn C.${program_name}_shader_desc(gfx.Backend) &gfx.ShaderDesc\n')
	}
}

// collect recursively collects `.glsl` file entries from `path` in `list`.
fn collect(path string, mut list []string) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or { return }
	for file in files {
		p := os.join_path(path, file)
		if os.is_dir(p) && !os.is_link(p) {
			collect(p, mut list)
		} else if os.exists(p) {
			if os.file_ext(p) == '.glsl' {
				list << os.real_path(p)
			}
		}
	}
	return
}

// ensure_external_tools returns nothing if the external
// tools can be setup or is already in place.
fn ensure_external_tools(opt Options) ! {
	if !os.exists(cache_dir) {
		os.mkdir_all(cache_dir)!
	}
	if opt.force_update {
		download_shdc(opt)!
		return
	}

	is_shdc_available := os.is_file(shdc_exe)
	is_shdc_executable := os.is_executable(shdc_exe)
	if opt.verbose {
		eprintln('reading version from ${shdc_version_file} ...')
		version := os.read_file(shdc_version_file) or { 'unknown' }
		eprintln('${tool_name} using sokol-shdc version ${version} at "${shdc_exe}"')
		eprintln('executable: ${is_shdc_executable}')
		eprintln(' available: ${is_shdc_available}')
		if is_shdc_available {
			eprintln(' file path: ${shdc_exe}')
			eprintln(' file size: ${os.file_size(shdc_exe)}')
			eprintln(' file time: ${time.unix_microsecond(os.file_last_mod_unix(shdc_exe),
				0)}')
		}
	}
	if is_shdc_available && is_shdc_executable {
		return
	}

	download_shdc(opt)!
}

// download_shdc downloads the `sokol-shdc` tool to an OS specific cache directory.
fn download_shdc(opt Options) ! {
	// We want to use the same, runtime, OS type as this tool is invoked on.
	mut download_url := shdc_urls[runtime_os] or { '' }
	$if arm64 && macos {
		download_url = shdc_urls['osx_a64']
	}
	if download_url == '' {
		return error('${tool_name} failed to download an external dependency "sokol-shdc" for ${runtime_os}.\nThe supported host platforms for shader compilation is ${supported_hosts}')
	}
	if opt.verbose {
		eprintln('> reading version from ${shdc_version_file} ...')
	}
	update_to_shdc_version := os.read_file(shdc_version_file) or { shdc_version }
	if opt.verbose {
		eprintln('> update_to_shdc_version: ${update_to_shdc_version} | shdc_version: ${shdc_version}')
	}
	if opt.verbose {
		if shdc_version != update_to_shdc_version && os.exists(shdc_exe) {
			eprintln('${tool_name} updating sokol-shdc to version ${shdc_version} ...')
		} else {
			eprintln('${tool_name} installing sokol-shdc version ${update_to_shdc_version} ...')
		}
	}
	if os.exists(shdc_exe) {
		os.rm(shdc_exe)!
	}

	mut dtmp_file, dtmp_path := util.temp_file(util.TempFileOptions{ path: os.dir(shdc_exe) })!
	dtmp_file.close()
	if opt.verbose {
		eprintln('${tool_name} downloading sokol-shdc from ${download_url}')
	}
	http.download_file(download_url, dtmp_path) or {
		os.rm(dtmp_path)!
		return error('${tool_name} failed to download sokol-shdc needed for shader compiling: ${err}')
	}
	// Make it executable
	os.chmod(dtmp_path, 0o775)!
	// Move downloaded file in place
	os.mv(dtmp_path, shdc_exe)!
	// Update internal version file
	os.write_file(shdc_version_file, shdc_version)!
}
