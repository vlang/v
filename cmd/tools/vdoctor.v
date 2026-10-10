import os
import time
import term
import v.util.version
import runtime

// compiled_vroot is the V root folder that the compiler resolved, when it compiled this tool:
// `builtin`, `os` and the other modules in this executable come from its `vlib` folder.
// Root selection depends on the compiler mode: source/cwd resolution and the macOS
// dispatcher's executable/built-in root can select different checkouts.
const compiled_vroot = @VEXEROOT

struct App {
mut:
	report_lines   []string
	cached_cpuinfo map[string]string
	vexe           string
}

fn (mut a App) println(s string) {
	a.report_lines << s
}

fn (mut a App) collect_info() {
	a.line('V full version', version.full_v_version(true))
	a.line(':-------------------', ':-------------------')

	mut os_kind := os.user_os()
	mut arch_details := []string{}
	arch_details << '${runtime.nr_cpus()} cpus'
	if runtime.is_32bit() {
		arch_details << '32bit'
	}
	if runtime.is_64bit() {
		arch_details << '64bit'
	}
	if runtime.is_big_endian() {
		arch_details << 'big endian'
	}
	if runtime.is_little_endian() {
		arch_details << 'little endian'
	}
	if os_kind == 'macos' {
		arch_details << a.cmd(command: ['sysctl', '-n', 'machdep.cpu.brand_string'])
	}
	if os_kind == 'linux' {
		mut cpu_details := ''
		if cpu_details == '' {
			cpu_details = a.cpu_info('model name')
		}
		if cpu_details == '' {
			cpu_details = a.cpu_info('hardware')
		}
		if cpu_details == '' {
			cpu_details = os.uname().machine
		}
		arch_details << cpu_details
	}
	if os_kind == 'windows' {
		arch_details << a.cmd(
			command: ['wmic', 'cpu', 'get', 'name', '/format:table']
			line:    2
		)
	}

	mut os_details := ''
	wsl_check := a.cmd(command: ['cat', '/proc/sys/kernel/osrelease'])
	if os_kind == 'linux' {
		os_details = a.get_linux_os_name()
		if a.cpu_info('flags').contains('hypervisor') {
			if wsl_check.contains('microsoft') {
				// WSL 2 is a Managed VM and Full Linux Kernel
				// See https://docs.microsoft.com/en-us/windows/wsl/compare-versions
				os_details += ' (WSL 2)'
			} else {
				os_details += ' (VM)'
			}
		}
		// WSL 1 is NOT a Managed VM and Full Linux Kernel
		// See https://docs.microsoft.com/en-us/windows/wsl/compare-versions
		if wsl_check.contains('Microsoft') {
			os_details += ' (WSL)'
		}
		// From https://unix.stackexchange.com/a/14346
		awk_cmd := '[ "$(awk \'\$5=="/" {print \$1}\' </proc/1/mountinfo)" != "$(awk \'\$5=="/" {print \$1}\' </proc/$$/mountinfo)" ] ; echo \$?'
		if a.cmd(command: ['sh', '-c', awk_cmd]) == '0' {
			os_details += ' (chroot)'
		}
	} else if os_kind == 'macos' {
		mut details := []string{}
		details << a.cmd(command: ['sw_vers', '-productName'])
		details << a.cmd(command: ['sw_vers', '-productVersion'])
		details << a.cmd(command: ['sw_vers', '-buildVersion'])
		os_details = details.join(', ')
	} else if os_kind == 'windows' {
		wmic_info := a.cmd(
			command: ['wmic', 'os', 'get', '*', '/format:value']
			line:    -1
		)
		p := a.parse(wmic_info, '=')
		mut caption, mut build_number, mut os_arch := p['caption'], p['buildnumber'], p['osarchitecture']
		os_details = '${caption} ${build_number} ${os_arch}'
	} else {
		ouname := os.uname()
		os_details = '${ouname.release}, ${ouname.version}'
	}
	a.line('OS', '${os_kind}, ${os_details}')
	a.line('Processor', arch_details.join(', '))
	total_memory := f32(runtime.total_memory() or { 0 }) / (1024.0 * 1024.0 * 1024.0)
	free_memory := f32(runtime.free_memory() or { 0 }) / (1024.0 * 1024.0 * 1024.0)
	if total_memory != 0 && free_memory != 0 {
		a.line('Memory', '${free_memory:.2}GB/${total_memory:.2}GB')
	} else {
		a.line('Memory', 'N/A')
	}

	a.line('', '')
	vexe := a.vexe
	vroot := os.dir(vexe)
	vmodules := os.vmodules_dir()
	vtmp_dir := os.vtmp_dir()
	getwd := os.getwd()
	os.chdir(vroot) or {}
	a.line('V executable', vexe)
	a.line('V last modified time', time.unix(os.file_last_mod_unix(vexe)).str())
	a.line('', '')
	a.line2('V home dir', diagnose_dir(vroot), vroot)
	a.report_vlib('V', vroot, compiled_vroot, vcurrent_hash())
	a.line2('VMODULES', diagnose_dir(vmodules), vmodules)
	a.line2('VTMP', diagnose_dir(vtmp_dir), vtmp_dir)
	a.line2('Current working dir', diagnose_dir(getwd), getwd)
	cwd_vroot := vroot_of(getwd)
	if cwd_vroot != '' && !is_same_dir(cwd_vroot, compiled_vroot) {
		// Also inspect a checkout that source/cwd module resolution can select.
		a.report_vlib('cwd', vroot, cwd_vroot, vcurrent_hash())
	}
	a.line('', '')

	a.line_env('VFLAGS')
	a.line_env('CFLAGS')
	a.line_env('LDFLAGS')

	a.line('Git version', a.cmd(command: ['git', '--version']))
	a.line('V git status', a.git_info())
	a.line('.git/config present', os.is_file('.git/config').str())
	a.line('', '')
	a.line('cc version', a.cmd(command: ['cc', '--version']))
	if os_kind == 'openbsd' {
		a.line('gcc version', a.cmd(command: ['egcc', '--version']))
	} else {
		a.line('gcc version', a.cmd(command: ['gcc', '--version']))
	}
	a.line('clang version', a.cmd(command: ['clang', '--version']))
	if os_kind == 'windows' {
		// Check for MSVC on windows
		a.line('msvc version', a.cmd(command: ['cl']))
	}
	a.report_tcc_version('thirdparty/tcc')
	a.line('emcc version', a.cmd(command: ['emcc', '--version']))
	if os_kind != 'openbsd' && os_kind != 'freebsd' {
		a.line('glibc version', a.cmd(command: ['ldd', '--version']))
	} else {
		a.line('glibc version', 'N/A')
	}
}

struct CmdConfig {
	line    int
	command []string
}

fn (mut a App) cmd(c CmdConfig) string {
	x := os.exec(c.command)
	if doctor_command_is_unavailable(x, os.user_os()) {
		return 'N/A'
	}
	if x.exit_code == 0 {
		if c.line < 0 {
			return x.output
		}
		output := x.output.split_into_lines()
		if output.len > 0 && output.len > c.line {
			return output[c.line]
		}
	}
	return 'Error: ${x.output}'
}

fn doctor_command_is_unavailable(x os.Result, os_kind string) bool {
	if x.exit_code < 0 || x.exit_code == 127 || (os_kind == 'windows' && x.exit_code == 1) {
		return true
	}
	// Windows reports missing executables and paths as CreateProcess errors.
	// A tool that started successfully can also exit with 2 or 3, so keep its errors.
	return os_kind == 'windows' && x.exit_code in [2, 3]
		&& x.output.starts_with('exec failed (CreateProcess) with code ${x.exit_code}:')
}

fn (mut a App) line(label string, value string) {
	a.println('|${label:-20}|${term.colorize(term.bold, value)}')
}

fn (mut a App) line2(label string, value string, value2 string) {
	a.println('|${label:-20}|${term.colorize(term.bold, value)}, value: ${term.colorize(term.bold, value2)}')
}

fn (mut a App) line_env(env_var string) {
	value := os.getenv(env_var)
	if value != '' {
		a.line('env ${env_var}', '"${value}"')
	}
}

fn (app &App) parse(config string, sep string) map[string]string {
	mut m := map[string]string{}
	lines := config.split_into_lines()
	for line in lines {
		sline := line.trim_space()
		if sline.len == 0 || sline[0] == `#` {
			continue
		}
		x := sline.split(sep)
		if x.len < 2 {
			continue
		}
		m[x[0].trim_space().to_lower()] = x[1].trim_space().trim('"')
	}
	return m
}

fn (mut a App) get_linux_os_name() string {
	if os.is_file('/etc/os-release') {
		if lines := os.read_file('/etc/os-release') {
			vals := a.parse(lines, '=')
			if vals['PRETTY_NAME'] != '' {
				return vals['PRETTY_NAME']
			}
		}
	}
	if os.exists_in_system_path('lsb_release') {
		return a.cmd(command: ['lsb_release', '-d', '-s'])
	}
	if os.is_file('/proc/version') {
		return a.cmd(command: ['cat', '/proc/version'])
	}
	ouname := os.uname()
	return '${ouname.release}, ${ouname.version}'
}

fn (mut a App) cpu_info(key string) string {
	if a.cached_cpuinfo.len > 0 {
		return a.cached_cpuinfo[key]
	}
	info := os.exec(['cat', '/proc/cpuinfo'])
	if info.exit_code != 0 {
		return '`cat /proc/cpuinfo` could not run'
	}
	a.cached_cpuinfo = a.parse(info.output, ':')
	return a.cached_cpuinfo[key]
}

fn (mut a App) git_info() string {
	// Check if in a Git repository
	x := os.exec(['git', 'rev-parse', '--is-inside-work-tree'])
	if x.exit_code != 0 || x.output.trim_space() != 'true' {
		return 'N/A'
	}
	mut out := a.cmd(
		command: ['git', '-C', '.', 'describe', '--abbrev=8', '--dirty', '--always', '--tags']
	).trim_space()
	os.exec(['git', '-C', '.', 'remote', 'add', 'V_REPO', 'https://github.com/vlang/v']) // ignore failure (i.e. remote exists)
	if '-skip-github' !in os.args {
		os.exec([a.vexe, 'timeout', '5.1', 'git -C . fetch V_REPO']) // usually takes ~0.6s; 5 seconds should be enough for even the slowest networks
	}
	commit_count := a.cmd(
		command: ['git', 'rev-list', '@{0}...V_REPO/master', '--right-only', '--count']
	).int()
	if commit_count > 0 {
		out += ' (${commit_count} commit(s) behind V master)'
	}
	return out
}

fn (mut a App) report_tcc_version(tccfolder string) {
	cmd := os.join_path(tccfolder, 'tcc.exe') + ' -v'
	x := os.exec(os.split_args(cmd) or { panic(err) })
	if x.exit_code == 0 {
		a.line('tcc version', '${x.output.trim_space()}')
	} else {
		a.line('tcc version', 'N/A')
	}
	if !os.is_file(os.join_path(tccfolder, '.git', 'config')) {
		a.line('tcc git status', 'N/A')
	} else {
		tcc_branch_name := a.cmd(
			command: ['git', '-C', tccfolder, 'rev-parse', '--abbrev-ref', 'HEAD']
		)
		tcc_commit := a.cmd(
			command: ['git', '-C', tccfolder, 'describe', '--abbrev=8', '--dirty', '--always',
				'--tags']
		)
		a.line('tcc git status', '${tcc_branch_name} ${tcc_commit}')
	}
}

fn (mut a App) report_info() {
	for x in a.report_lines {
		println(x)
	}
}

fn is_writable_dir(path string) bool {
	os.ensure_folder_is_writable(path) or { return false }
	return true
}

fn diagnose_dir(path string) string {
	mut diagnostics := []string{}
	if !is_writable_dir(path) {
		diagnostics << 'NOT writable'
	}
	if path.contains(' ') {
		diagnostics << 'contains spaces'
	}
	path_non_ascii_runes := path.runes().filter(it > 255)
	if path_non_ascii_runes.len > 0 {
		diagnostics << 'contains these non ASCII characters: ${path_non_ascii_runes}'
	}
	if diagnostics.len == 0 {
		diagnostics << 'OK'
	}
	return diagnostics.join(', ')
}

// report_vlib adds the rows about a `vlib` that the compiler pairs itself with: where it is,
// and whether its checkout is at the commit that the compiler was built from. A compiler that
// is older or newer than its `vlib` still runs, but it can behave differently, silently.
fn (mut a App) report_vlib(label string, vexe_dir string, vlib_root string, build_commit string) {
	vlib_dir := os.join_path(vlib_root, 'vlib')
	vlib_commit := version.githash(vlib_root) or { '' }
	a.line2('${label} vlib dir', diagnose_vlib_dir(vlib_root, vexe_dir), vlib_dir)
	a.line('${label} vlib commit', diagnose_vlib_commit(build_commit, vlib_commit))
}

// vroot_of returns the V root folder that `dir` is in: the nearest folder upwards that has
// `vlib/builtin`. It returns '' when there is none. The compiler finds the `vlib` for the
// sources that it compiles this way in source/cwd resolution modes; the macOS dispatcher
// can instead retain the invoking compiler's root. See `nearest_vroot_for_path` in `v.driver`.
fn vroot_of(dir string) string {
	mut current := os.real_path(dir)
	for _ in 0 .. 8 {
		if os.is_dir(os.join_path(current, 'vlib', 'builtin')) {
			return current
		}
		current = os.parent_dir(current)
		if current == '' {
			break
		}
	}
	return ''
}

// diagnose_vlib_dir tells whether `vlib_root`, the V root folder whose `vlib` the compiler
// uses, is the folder of the V executable. A copied or moved executable can use
// the `vlib` of the checkout that it was built in.
fn diagnose_vlib_dir(vlib_root string, vexe_dir string) string {
	if is_same_dir(vlib_root, vexe_dir) {
		return 'OK'
	}
	return 'NOT in the folder of the V executable'
}

// diagnose_vlib_commit compares the commit that the compiler was built from, with the commit
// that the checkout of its `vlib` is at now. They differ when that checkout was updated without
// rebuilding V, and when the executable comes from another checkout. Uncommitted changes do
// not count. An empty commit is one that could not be read, like outside of a Git checkout.
fn diagnose_vlib_commit(build_commit string, vlib_commit string) string {
	if build_commit == '' || vlib_commit == '' {
		return 'N/A'
	}
	if build_commit == vlib_commit {
		return 'OK, value: ${vlib_commit}'
	}
	return 'MISMATCH: V was built from commit ${build_commit}, but this vlib is at commit ${vlib_commit}'
}

// is_same_dir tells whether both paths name the same folder.
fn is_same_dir(a string, b string) bool {
	mut real_a := os.real_path(a).replace('\\', '/').trim_right('/')
	mut real_b := os.real_path(b).replace('\\', '/').trim_right('/')
	$if windows {
		real_a = real_a.to_lower()
		real_b = real_b.to_lower()
	}
	return real_a == real_b
}

fn main() {
	mut app := App{}
	app.vexe = os.getenv('VEXE')
	app.collect_info()
	app.report_info()
}
