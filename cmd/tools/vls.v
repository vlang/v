// Copyright (c) 2022 Ned Palacios. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// The V language server launcher and updater utility is
// a program responsible for installing, updating, and
// executing the V language server program with the primary
// goal of simplifying the installation process across
// all different platforms, text editors, and IDEs.
module main

import os
import flag
import x.json2
import net.http
import runtime
import crypto.sha256
import time
import json

enum UpdateSource {
	github_releases
	git_repo
}

enum SetupKind {
	none_
	install
	update
}

enum OutputMode {
	silent
	text
	json
}

struct VlsUpdater {
mut:
	output        OutputMode   = .text
	setup_kind    SetupKind    = .none_
	update_source UpdateSource = .github_releases
	ls_path       string // --path
	pass_to_ls    bool   // --ls
	is_check      bool   // --check
	is_force      bool   // --force
	is_help       bool   // --help
	args          []string
}

const vls_folder = os.join_path(os.home_dir(), '.vls')

const vls_bin_folder = os.join_path(vls_folder, 'bin')

const vls_cache_folder = os.join_path(vls_folder, '.cache')

const vls_manifest_path = os.join_path(vls_folder, 'vls.config.json')

const vls_src_folder = os.join_path(vls_folder, 'src')

const server_not_found_err = error_with_code('Language server is not installed nor found.',
	101)

const json_enc = json2.Encoder{
	newline: `\n`
	newline_spaces_count: 2
	escape_unicode: false
}

fn (upd VlsUpdater) check_or_create_vls_folder() ! {
	if !os.exists(vls_folder) {
		upd.log('Creating .vls folder...')
		os.mkdir(vls_folder)!
	}
}

fn (upd VlsUpdater) manifest_config() !map[string]json2.Any {
	manifest_buf := os.read_file(vls_manifest_path) or { '{}' }
	manifest_contents := json2.raw_decode(manifest_buf)!.as_map()
	return manifest_contents
}

fn (upd VlsUpdater) exec_asset_file_name() string {
	// TODO: support for Arm and other archs
	os_name := os.user_os()
	arch := if runtime.is_64bit() { 'x64' } else { 'x86' }
	ext := if os_name == 'windows' { '.exe' } else { '' }
	return 'vls_${os_name}_${arch + ext}'
}

fn (upd VlsUpdater) update_manifest(new_path string, from_source bool, timestamp time.Time) ! {
	upd.log('Updating permissions...')
	os.chmod(new_path, 0o755)!

	upd.log('Updating vls.config.json...')
	mut manifest := upd.manifest_config() or {
		map[string]json2.Any{}
	}

	$if macos {
		if os.exists(vls_manifest_path) {
			os.rm(vls_manifest_path) or {}
		}
	}

	mut manifest_file := os.open_file(vls_manifest_path, 'w+')!
	defer {
		manifest_file.close()
	}

	manifest['server_path'] = json2.Any(new_path)
	manifest['last_updated'] = json2.Any(timestamp.format_ss())
	manifest['from_source'] = json2.Any(from_source)

	json_enc.encode_value(manifest, mut manifest_file)!
}

fn (upd VlsUpdater) init_download_prebuilt() ! {
	if !os.exists(vls_cache_folder) {
		os.mkdir(vls_cache_folder)!
	}

	if os.exists(vls_bin_folder) {
		os.rmdir_all(vls_bin_folder)!
	}

	os.mkdir(vls_bin_folder)!
}

fn (upd VlsUpdater) get_last_updated_at() !time.Time {
	if manifest := upd.manifest_config() {
		if 'last_updated' in manifest {
			return time.parse(manifest['last_updated'] or { '' }.str()) or { return error('none') }
		}
	}
	return error('none')
}

fn (upd VlsUpdater) download_prebuilt() ! {
	mut has_last_updated_at := true
	last_updated_at := upd.get_last_updated_at() or {
		has_last_updated_at = false
		time.now()
	}
	defer {
		os.rmdir_all(vls_cache_folder) or {}
	}

	upd.log('Finding prebuilt executables from GitHub release..')
	resp := http.get('https://api.github.com/repos/vlang/vls/releases')!
	releases_json := json2.raw_decode(resp.body)!.arr()
	if releases_json.len == 0 {
		return error('Unable to fetch latest VLS release data: No releases found.')
	}

	latest_release := releases_json[0].as_map()
	assets := latest_release['assets']!.arr()

	mut checksum_asset_idx := -1
	mut exec_asset_idx := -1

	exp_asset_name := upd.exec_asset_file_name()
	exec_asset_file_path := os.join_path(vls_cache_folder, exp_asset_name)

	for asset_idx, raw_asset in assets {
		asset := raw_asset.as_map()
		t_asset := asset['name'] or { return }
		match t_asset.str() {
			exp_asset_name {
				exec_asset_idx = asset_idx

				// check timestamp here
			}
			'checksums.txt' {
				checksum_asset_idx = asset_idx
			}
			else {}
		}
	}

	if exec_asset_idx == -1 {
		return error_with_code('No executable found for this system.', 100)
	} else if checksum_asset_idx == -1 {
		return error('Unable to download executable: missing checksum')
	}

	exec_asset := assets[exec_asset_idx].as_map()

	mut asset_last_updated_at := time.now()
	if created_at := exec_asset['created_at'] {
		asset_last_updated_at = time.parse_rfc3339(created_at.str()) or { asset_last_updated_at }
	}

	if has_last_updated_at && !upd.is_force && asset_last_updated_at <= last_updated_at {
		upd.log("VLS was already updated to it's latest version.")
		return
	}

	upd.log('Executable found for this system. Downloading...')
	upd.init_download_prebuilt()!
	http.download_file(exec_asset['browser_download_url']!.str(), exec_asset_file_path)!

	checksum_file_path := os.join_path(vls_cache_folder, 'checksums.txt')
	checksum_file_asset := assets[checksum_asset_idx].as_map()
	http.download_file(checksum_file_asset['browser_download_url']!.str(), checksum_file_path)!
	checksums := os.read_file(checksum_file_path)!.split_into_lines()

	upd.log('Verifying checksum...')
	for checksum_result in checksums {
		if checksum_result.ends_with(exp_asset_name) {
			checksum := checksum_result.split(' ')[0]
			actual := calculate_checksum(exec_asset_file_path) or { '' }
			if checksum != actual {
				return error('Downloaded executable is corrupted. Exiting...')
			}
			break
		}
	}

	new_exec_path := os.join_path(vls_bin_folder, exp_asset_name)
	os.cp(exec_asset_file_path, new_exec_path)!
	upd.update_manifest(new_exec_path, false, asset_last_updated_at) or {
		upd.log('Unable to update config but the executable was updated successfully.')
	}
	upd.print_new_vls_version(new_exec_path)
}

fn (upd VlsUpdater) print_new_vls_version(new_vls_exec_path string) {
	exec_version := os.execute('${new_vls_exec_path} --version')
	if exec_version.exit_code == 0 {
		upd.log('VLS was updated to version: ${exec_version.output.all_after('vls version ').trim_space()}')
	}
}

fn calculate_checksum(file_path string) !string {
	data := os.read_file(file_path)!
	return sha256.hexhash(data)
}

fn (upd VlsUpdater) compile_from_source() ! {
	git := os.find_abs_path_of_executable('git') or { return error('Git not found.') }

	if !os.exists(vls_src_folder) {
		upd.log('Cloning VLS repo...')
		clone_result := os.execute('${git} clone https://github.com/nedpals/vls ${vls_src_folder}')
		if clone_result.exit_code != 0 {
			return error('Failed to build VLS from source. Reason: ${clone_result.output}')
		}
	} else {
		upd.log('Updating VLS repo...')
		pull_result := os.execute('${git} -C ${vls_src_folder} pull')
		if !upd.is_force && pull_result.output.trim_space() == 'Already up to date.' {
			upd.log("VLS was already updated to it's latest version.")
			return
		}
	}

	upd.log('Compiling VLS from source...')
	possible_compilers := ['cc', 'gcc', 'clang', 'msvc']
	mut selected_compiler_idx := -1

	for i, cname in possible_compilers {
		os.find_abs_path_of_executable(cname) or { continue }
		selected_compiler_idx = i
		break
	}

	if selected_compiler_idx == -1 {
		return error('Cannot compile VLS from source: no appropriate C compiler found.')
	}

	compile_result := os.execute('v run ${os.join_path(vls_src_folder, 'build.vsh')} ${possible_compilers[selected_compiler_idx]}')
	if compile_result.exit_code != 0 {
		return error('Cannot compile VLS from source: ${compile_result.output}')
	}

	exec_path := os.join_path(vls_src_folder, 'bin', 'vls')
	upd.update_manifest(exec_path, true, time.now()) or {
		upd.log('Unable to update config but the executable was updated successfully.')
	}
	upd.print_new_vls_version(exec_path)
}

fn (upd VlsUpdater) find_ls_path() !string {
	manifest := upd.manifest_config()!
	if 'server_path' in manifest {
		server_path := manifest['server_path'] or { return error('none') }
		if server_path is string {
			if server_path.len == 0 {
				return error('none')
			}

			return server_path
		}
	}
	return error('none')
}

fn (mut upd VlsUpdater) parse(mut fp flag.FlagParser) ! {
	is_json := fp.bool('json', ` `, false, 'Print the output as JSON.')
	if is_json {
		upd.output = .json
	}

	is_silent := fp.bool('silent', ` `, false, 'Disables output printing.')
	if is_silent && is_json {
		return error('Cannot use --json and --silent at the same time.')
	} else if is_silent {
		upd.output = .silent
	}

	is_install := fp.bool('install', ` `, false, 'Installs the language server. You may also use this flag to re-download or force update your existing installation.')
	is_update := fp.bool('update', ` `, false, 'Updates the installed language server.')
	upd.is_check = fp.bool('check', ` `, false, 'Checks if the language server is installed.')
	upd.is_force = fp.bool('force', ` `, false, 'Force install or update the language server.')
	is_source := fp.bool('source', ` `, false, 'Clone and build the language server from source.')

	if is_install && is_update {
		return error('Cannot use --install and --update at the same time.')
	} else if is_install {
		upd.setup_kind = .install
	} else if is_update {
		upd.setup_kind = .update
	}

	if is_source {
		upd.update_source = .git_repo
	}

	upd.pass_to_ls = fp.bool('ls', ` `, false, 'Pass the arguments to the language server.')
	if ls_path := fp.string_opt('path', `p`, 'Path to the language server executable.') {
		if upd.setup_kind != .none_ {
			return error('Cannot use --install or --update when --path is supplied.')
		} else if !os.is_executable(ls_path) {
			return server_not_found_err
		}

		upd.ls_path = ls_path
	}

	upd.is_help = fp.bool('help', `h`, false, "Show this updater's help text. To show the help text for the language server, pass the `--ls` flag before it.")

	if !upd.is_help && !upd.pass_to_ls {
		// automatically set the cli launcher to language server mode
		upd.pass_to_ls = true
	}

	if upd.pass_to_ls {
		if upd.ls_path.len == 0 {
			if ls_path := upd.find_ls_path() {
				if !upd.is_force && upd.setup_kind == .install {
					return error_with_code('VLS was already installed.', 102)
				}

				upd.ls_path = ls_path
			} else if upd.setup_kind == .none_ {
				return server_not_found_err
			}
		}

		if upd.is_help {
			upd.args << '--help'
		}

		fp.allow_unknown_args()
		upd.args << fp.finalize() or { fp.remaining_parameters() }
	} else {
		fp.finalize()!
	}
}

fn (upd VlsUpdater) log(msg string) {
	match upd.output {
		.text {
			println('> ${msg}')
		}
		.json {
			print('{"message":"${msg}"}')
			flush_stdout()
		}
		.silent {}
	}
}

fn (upd VlsUpdater) error_details(err IError) string {
	match err.code() {
		101 {
			mut vls_dir_shortened := '\$HOME/.vls'
			$if windows {
				vls_dir_shortened = '%USERPROFILE%\\.vls'
			}

			return '
- If you are using this for the first time, please run
  `v ls --install` first to download and install VLS.
- If you are using a custom version of VLS, check if
  the specified path exists and is a valid executable.
- If you have an existing installation of VLS, be sure
  to remove "vls.config.json" and "bin" located inside
  "${vls_dir_shortened}" and re-install.

  If none of the options listed have solved your issue,
  please report it at https://github.com/vlang/v/issues
'
		}
		else {
			return ''
		}
	}
}

[noreturn]
fn (upd VlsUpdater) cli_error(err IError) {
	match upd.output {
		.text {
			eprintln('v ls error: ${err.msg()} (${err.code()})')
			if err !is none {
				eprintln(upd.error_details(err))
			}

			print_backtrace()
		}
		.json {
			print('{"error":{"message":${json.encode(err.msg())},"code":"${err.code()}","details":${json.encode(upd.error_details(err).trim_space())}}}')
			flush_stdout()
		}
		.silent {}
	}
	exit(1)
}

fn (upd VlsUpdater) check_installation() {
	if upd.ls_path.len == 0 {
		upd.log('Language server is not installed')
	} else {
		upd.log('Language server is installed at: ${upd.ls_path}')
	}
}

fn (upd VlsUpdater) run(fp flag.FlagParser) ! {
	if upd.is_check {
		upd.check_installation()
	} else if upd.setup_kind != .none_ {
		upd.check_or_create_vls_folder()!

		match upd.update_source {
			.github_releases {
				upd.download_prebuilt() or {
					if err.code() == 100 {
						upd.compile_from_source()!
					}
					return err
				}
			}
			.git_repo {
				upd.compile_from_source()!
			}
		}
	} else if upd.pass_to_ls {
		exit(os.system('${upd.ls_path} ${upd.args.join(' ')}'))
	} else if upd.is_help {
		println(fp.usage())
		exit(0)
	}
}

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	mut upd := VlsUpdater{}

	fp.application('v ls')
	fp.description('Installs, updates, and executes the V language server program')
	fp.version('0.1')

	// just to make sure whenever user wants to
	// interact directly with the executable
	// instead of the usual `v ls` command
	if fp.args.len >= 2 && fp.args[0..2] == [os.executable(), 'ls'] {
		// skip the executable here, the next skip_executable
		// outside the if statement will skip the `ls` part
		fp.skip_executable()
	}

	// skip the executable or the `ls` part
	fp.skip_executable()

	upd.parse(mut fp) or {
		if err.code() == 102 {
			upd.log(err.msg())
			exit(0)
		} else {
			upd.cli_error(err)
		}
	}

	upd.run(fp) or { upd.cli_error(err) }
}
