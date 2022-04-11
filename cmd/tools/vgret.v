// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// vgret (V Graphics REgression Tool) aids in generating screenshots of various graphical `gg`
// based V applications, in a structured directory hierarchy, with the intent of either:
// * Generate a directory structure of screenshots/images to test against
//   (which, as an example, could later be pushed to a remote git repository)
// * Test for *visual* differences between two, structurally equal, directories
//
// vgret uses features and applications that is currently only available on Linux based distros:
// idiff : `sudo apt install openimageio-tools` to programmatically find *visual* differences between two images.
//
// For developers:
// For a quick overview of the generated images you can use `montage` from imagemagick to generate a "Contact Sheet":
// montage -verbose -label '%f' -font Helvetica -pointsize 10 -background '#000000' -fill 'gray' -define jpeg:size=200x200 -geometry 200x200+2+2 -auto-orient $(fd -t f . /path/to/vgret/out/dir) /tmp/montage.jpg
//
// To generate the reference images locally - or for uploading to a remote repo like `gg-regression-images`
// You can do the following:
// 1. `export DISPLAY=:99`                            # Start all graphical apps on DISPLAY 99
// 2. `Xvfb $DISPLAY -screen 0 1280x1024x24 &`        # Starts a virtual X11 screen buffer
// 3. `v gret -v /tmp/gg-regression-images`           # Generate reference images to /tmp/gg-regression-images
// 4. `v gret -v /tmp/test /tmp/gg-regression-images` # Test if the tests can pass locally by comparing to a fresh imageset
// 5. Visually check the images (you can get an overview by running the `montage` command above)
// 6. Upload to GitHub or keep locally for more testing/tweaking
//
// It's a known factor that the images generated on a local machine won't match the images generated on a remote machine by 100%.
// They will most likely differ by a small percentage - the comparison tool can be tweaked to accept these subtle changes,
// at the expense of slightly more inaccurate test results. For non-animated apps the percentage should be > 0.01.
// You can emulate or test these inaccuracies to some extend locally by simply running the test from a terminal using
// your physical X11 session display (Usually DISPLAY=:0).
//
// Read more about the options of `idiff` here: https://openimageio.readthedocs.io/en/latest/idiff.html
//
import os
import flag
import toml

const (
	tool_name        = os.file_name(os.executable())
	tool_version     = '0.0.1'
	tool_description = '\n  Dump and/or compare rendered frames of `gg` based apps

Examples:
  Generate screenshots to `/tmp/test`
    v gret /tmp/test
  Generate and compare screenshots in `/tmp/src` to existing screenshots in `/tmp/dst`
    v gret /tmp/src /tmp/dst
  Compare screenshots in `/tmp/src` to existing screenshots in `/tmp/dst`
    v gret --compare-only /tmp/src /tmp/dst
'
	tmp_dir    = os.join_path(os.temp_dir(), 'v', tool_name)
	runtime_os = os.user_os()
	v_root     = os.real_path(@VMODROOT)
)

const (
	supported_hosts = ['linux']
	// External tool executables
	v_exe           = vexe()
	idiff_exe       = os.find_abs_path_of_executable('idiff') or { '' }
)

const (
	embedded_toml    = $embed_file('vgret.defaults.toml', .zlib)
	default_toml     = embedded_toml.to_string()
	empty_toml_array = []toml.Any{}
	empty_toml_map   = map[string]toml.Any{}
)

struct Config {
	path string
mut:
	apps []AppConfig
}

struct CompareOptions {
mut:
	method string = 'idiff'
	flags  []string
}

struct CaptureOptions {
mut:
	method string = 'gg_record'
	flags  []string
	env    map[string]string
}

struct AppConfig {
	compare  CompareOptions
	capture  CaptureOptions
	path     string
	abs_path string
mut:
	screenshots_path string
	screenshots      []string
}

struct Options {
	verbose      bool
	compare_only bool
	root_path    string
mut:
	config Config
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
	fp.arguments_description('PATH [PATH]')
	fp.skip_executable()

	show_help := fp.bool('help', `h`, false, 'Show this help text.')
	if show_help {
		println(fp.usage())
		exit(0)
	}

	// Collect tool options
	mut opt := Options{
		verbose: fp.bool('verbose', `v`, false, "Be verbose about the tool's progress.")
		compare_only: fp.bool('compare-only', `c`, false, "Don't generate screenshots - only compare input directories")
		root_path: fp.string('root-path', `r`, v_root, 'Root path of the comparison')
	}

	toml_conf := fp.string('toml-config', `t`, default_toml, 'Path or string with TOML configuration')

	ensure_env(opt) or { panic(err) }

	arg_paths := fp.finalize() or { panic(err) }

	if arg_paths.len == 0 {
		println(fp.usage())
		println('\nError missing arguments')
		exit(1)
	}

	opt.config = new_config(opt.root_path, toml_conf) ?

	gen_in_path := arg_paths[0]
	if arg_paths.len >= 1 {
		generate_screenshots(mut opt, gen_in_path) ?
	}
	if arg_paths.len > 1 {
		target_path := arg_paths[1]
		path := opt.config.path
		all_paths_in_use := [path, gen_in_path, target_path]
		for path_in_use in all_paths_in_use {
			if !os.is_dir(path_in_use) {
				panic('`$path_in_use` is not a directory')
			}
		}
		if path == target_path || gen_in_path == target_path || gen_in_path == path {
			panic('Compare paths can not be the same directory `$path`/`$target_path`/`$gen_in_path`')
		}
		compare_screenshots(opt, gen_in_path, target_path) or { panic(err) }
	}
}

fn generate_screenshots(mut opt Options, output_path string) ? {
	path := opt.config.path

	dst_path := output_path.trim_right('/')

	if !os.is_dir(path) {
		return error('`$path` is not a directory')
	}

	for mut app_config in opt.config.apps {
		file := app_config.path
		app_path := app_config.abs_path

		mut rel_out_path := ''
		if os.is_file(app_path) {
			rel_out_path = os.dir(file)
		} else {
			rel_out_path = file
		}

		if opt.verbose {
			eprintln('Compiling shaders (if needed) for `$file`')
		}
		sh_result := os.execute('${os.quoted_path(v_exe)} shader ${os.quoted_path(app_path)}')
		if sh_result.exit_code != 0 {
			if opt.verbose {
				eprintln('Skipping shader compile for `$file` v shader failed with:\n$sh_result.output')
			}
			continue
		}

		if !os.exists(dst_path) {
			if opt.verbose {
				eprintln('Creating output path `$dst_path`')
			}
			os.mkdir_all(dst_path) ?
		}

		screenshot_path := os.join_path(dst_path, rel_out_path)
		if !os.exists(screenshot_path) {
			os.mkdir_all(screenshot_path) or {
				return error('Failed making screenshot path `$screenshot_path`')
			}
		}

		app_config.screenshots_path = screenshot_path
		app_config.screenshots = take_screenshots(opt, app_config) or {
			return error('Failed taking screenshots of `$app_path`:\n$err.msg()')
		}
	}
}

fn compare_screenshots(opt Options, output_path string, target_path string) ? {
	mut fails := map[string]string{}
	mut warns := map[string]string{}
	for app_config in opt.config.apps {
		screenshots := app_config.screenshots
		if opt.verbose {
			eprintln('Comparing $screenshots.len screenshots in `$output_path` with `$target_path`')
		}
		for screenshot in screenshots {
			relative_screenshot := screenshot.all_after(output_path + os.path_separator)

			src := screenshot
			target := os.join_path(target_path, relative_screenshot)

			if opt.verbose {
				eprintln('Comparing `$src` with `$target` with $app_config.compare.method')
			}

			if app_config.compare.method == 'idiff' {
				if idiff_exe == '' {
					return error('$tool_name need the `idiff` tool installed. It can be installed on Ubuntu with `sudo apt install openimageio-tools`')
				}
				diff_file := os.join_path(os.temp_dir(), os.file_name(src).all_before_last('.') +
					'.diff.tif')
				flags := app_config.compare.flags.join(' ')
				diff_cmd := '${os.quoted_path(idiff_exe)} $flags -abs -od -o ${os.quoted_path(diff_file)} -abs ${os.quoted_path(src)} ${os.quoted_path(target)}'
				if opt.verbose {
					eprintln('Running: $diff_cmd')
				}

				result := os.execute(diff_cmd)

				if opt.verbose && result.exit_code == 0 {
					eprintln('OUTPUT: \n$result.output')
				}
				if result.exit_code != 0 {
					eprintln('OUTPUT: \n$result.output')
					if result.exit_code == 1 {
						warns[src] = target
					} else {
						fails[src] = target
					}
				}
			}
		}
	}

	if warns.len > 0 {
		eprintln('--- WARNINGS ---')
		eprintln('The following files had warnings when compared to their targets')
		for warn_src, warn_target in warns {
			eprintln('$warn_src ~= $warn_target')
		}
	}
	if fails.len > 0 {
		eprintln('--- ERRORS ---')
		eprintln('The following files did not match their targets')
		for fail_src, fail_target in fails {
			eprintln('$fail_src != $fail_target')
		}
		first := fails.keys()[0]
		fail_copy := os.join_path(os.temp_dir(), 'fail.' + first.all_after_last('.'))
		os.cp(first, fail_copy) or { panic(err) }
		eprintln('First failed file `$first` is copied to `$fail_copy`')

		diff_file := os.join_path(os.temp_dir(), os.file_name(first).all_before_last('.') +
			'.diff.tif')
		diff_copy := os.join_path(os.temp_dir(), 'diff.tif')
		if os.is_file(diff_file) {
			os.cp(diff_file, diff_copy) or { panic(err) }
			eprintln('First failed diff file `$diff_file` is copied to `$diff_copy`')
		}
		exit(1)
	}
}

fn take_screenshots(opt Options, app AppConfig) ?[]string {
	out_path := app.screenshots_path
	if !opt.compare_only {
		if opt.verbose {
			eprintln('Taking screenshot(s) of `$app.path` to `$out_path`')
		}

		if app.capture.method == 'gg_record' {
			for k, v in app.capture.env {
				rv := v.replace('\$OUT_PATH', out_path)
				if opt.verbose {
					eprintln('Setting ENV `$k` = $rv ...')
				}
				os.setenv('$k', rv, true)
			}

			mut flags := app.capture.flags.join(' ')
			v_cmd := '${os.quoted_path(v_exe)} $flags -d gg_record run ${os.quoted_path(app.abs_path)}'
			if opt.verbose {
				eprintln('Running `$v_cmd`')
			}
			result := os.execute('$v_cmd')
			if result.exit_code != 0 {
				return error('Failed taking screenshot of `$app.abs_path`:\n$result.output')
			}
		}
	}
	mut screenshots := []string{}
	shots := os.ls(out_path) or { return error('Failed listing dir `$out_path`') }
	for shot in shots {
		if shot.starts_with(os.file_name(app.path).all_before_last('.')) {
			screenshots << os.join_path(out_path, shot)
		}
	}
	return screenshots
}

// ensure_env returns nothing if everything is okay.
fn ensure_env(opt Options) ? {
	if !os.exists(tmp_dir) {
		os.mkdir_all(tmp_dir) ?
	}

	if runtime_os !in supported_hosts {
		return error('$tool_name is currently only supported on $supported_hosts hosts')
	}
}

// vexe returns the absolute path to the V compiler.
fn vexe() string {
	mut exe := os.getenv('VEXE')
	if os.is_executable(exe) {
		return os.real_path(exe)
	}
	possible_symlink := os.find_abs_path_of_executable('v') or { '' }
	if os.is_executable(possible_symlink) {
		exe = os.real_path(possible_symlink)
	}
	return exe
}

fn new_config(root_path string, toml_config string) ?Config {
	doc := if os.is_file(toml_config) {
		toml.parse_file(toml_config) ?
	} else {
		toml.parse_text(toml_config) ?
	}

	path := os.real_path(root_path).trim_right('/')

	compare_method := doc.value('compare.method').default_to('idiff').string()
	compare_flags := doc.value('compare.flags').default_to(empty_toml_array).array().as_strings()
	default_compare := CompareOptions{
		method: compare_method
		flags: compare_flags
	}
	capture_method := doc.value('capture.method').default_to('gg_record').string()
	capture_flags := doc.value('capture.flags').default_to(empty_toml_array).array().as_strings()
	capture_env := doc.value('capture.env').default_to(empty_toml_map).as_map()
	mut env_map := map[string]string{}
	for k, v in capture_env {
		env_map[k] = v.string()
	}
	default_capture := CaptureOptions{
		method: capture_method
		flags: capture_flags
		env: env_map
	}

	apps_any := doc.value('apps').default_to(empty_toml_array).array()
	mut apps := []AppConfig{cap: apps_any.len}
	for app_any in apps_any {
		rel_path := app_any.value('path').string().trim_right('/')

		// Merge, per app, overwrites
		mut merged_compare := CompareOptions{}
		merged_compare.method = app_any.value('compare.method').default_to(default_compare.method).string()
		merged_compare_flags := app_any.value('compare.flags').default_to(empty_toml_array).array().as_strings()
		if merged_compare_flags.len > 0 {
			merged_compare.flags = merged_compare_flags
		} else {
			merged_compare.flags = default_compare.flags
		}

		mut merged_capture := CaptureOptions{}
		merged_capture.method = app_any.value('capture.method').default_to(default_capture.method).string()
		merged_capture_flags := app_any.value('capture.flags').default_to(empty_toml_array).array().as_strings()
		if merged_capture_flags.len > 0 {
			merged_capture.flags = merged_capture_flags
		} else {
			merged_capture.flags = default_capture.flags
		}

		merge_capture_env := app_any.value('capture.env').default_to(empty_toml_map).as_map()
		mut merge_env_map := default_capture.env.clone()
		for k, v in merge_capture_env {
			merge_env_map[k] = v.string()
		}
		for k, v in merge_env_map {
			merged_capture.env[k] = v
		}

		app_config := AppConfig{
			compare: merged_compare
			capture: merged_capture
			path: rel_path
			abs_path: os.join_path(path, rel_path).trim_right('/')
		}
		apps << app_config
	}

	return Config{
		apps: apps
		path: path
	}
}
