import os
import time
import term
import v.util.version
import runtime

struct App {
mut:
	report_lines   []string
	cached_cpuinfo map[string]string
}

fn (mut a App) println(s string) {
	a.report_lines << s
}

fn (mut a App) collect_info() {
	a.line('V full version', version.full_v_version(true))

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
		arch_details << a.cmd(command: 'sysctl -n machdep.cpu.brand_string')
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
			command: 'wmic cpu get name /format:table'
			line:    1
		)
	}

	mut os_details := ''
	wsl_check := a.cmd(command: 'cat /proc/sys/kernel/osrelease')
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
		if a.cmd(command: awk_cmd) == '0' {
			os_details += ' (chroot)'
		}
	} else if os_kind == 'macos' {
		mut details := []string{}
		details << a.cmd(command: 'sw_vers -productName')
		details << a.cmd(command: 'sw_vers -productVersion')
		details << a.cmd(command: 'sw_vers -buildVersion')
		os_details = details.join(', ')
	} else if os_kind == 'windows' {
		wmic_info := a.cmd(
			command: 'wmic os get * /format:value'
			line:    -1
		)
		p := a.parse(wmic_info, '=')
		caption, build_number, os_arch := p['caption'], p['buildnumber'], p['osarchitecture']
		os_details = '${caption} v${build_number} ${os_arch}'
	} else {
		ouname := os.uname()
		os_details = '${ouname.release}, ${ouname.version}'
	}
	a.line('OS', '${os_kind}, ${os_details}')
	a.line('Processor', arch_details.join(', '))
	a.println('')
	getwd := os.getwd()
	vmodules := os.vmodules_dir()
	vtmp_dir := os.vtmp_dir()
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	a.line('getwd', getwd)
	a.line('vexe', vexe)
	a.line('vexe mtime', time.unix(os.file_last_mod_unix(vexe)).str())
	a.println('')
	a.line2('vroot', diagnose_dir(vroot), vroot)
	a.line2('VMODULES', diagnose_dir(vmodules), vmodules)
	a.line2('VTMP', diagnose_dir(vtmp_dir), vtmp_dir)
	vflags := os.getenv('VFLAGS')
	a.println('')
	if vflags != '' {
		a.line('env VFLAGS', '"${vflags}"')
		a.println('')
	}
	a.line('Git version', a.cmd(command: 'git --version'))
	a.line('Git vroot status', a.git_info())
	a.line('.git/config present', os.is_file('.git/config').str())
	a.println('')

	a.line('CC version', a.cmd(command: 'cc --version'))
	a.report_tcc_version('thirdparty/tcc')
}

struct CmdConfig {
	line    int
	command string
}

fn (mut a App) cmd(c CmdConfig) string {
	x := os.execute(c.command)
	if x.exit_code < 0 {
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

fn (mut a App) line(label string, value string) {
	a.println('${label}: ${term.colorize(term.bold, value)}')
}

fn (mut a App) line2(label string, value string, value2 string) {
	a.println('${label}: ${term.colorize(term.bold, value)}, value: ${term.colorize(term.bold,
		value2)}')
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
	mut os_details := ''
	linux_os_methods := ['os-release', 'lsb_release', 'kernel', 'uname']
	for m in linux_os_methods {
		match m {
			'os-release' {
				if !os.is_file('/etc/os-release') {
					continue
				}
				lines := os.read_file('/etc/os-release') or { continue }
				vals := a.parse(lines, '=')
				if vals['PRETTY_NAME'] == '' {
					continue
				}
				os_details = vals['PRETTY_NAME']
				break
			}
			'lsb_release' {
				exists := a.cmd(command: 'type lsb_release')
				if exists.starts_with('Error') {
					continue
				}
				os_details = a.cmd(command: 'lsb_release -d -s')
				break
			}
			'kernel' {
				if !os.is_file('/proc/version') {
					continue
				}
				os_details = a.cmd(command: 'cat /proc/version')
				break
			}
			'uname' {
				ouname := os.uname()
				os_details = '${ouname.release}, ${ouname.version}'
				break
			}
			else {}
		}
	}
	return os_details
}

fn (mut a App) cpu_info(key string) string {
	if a.cached_cpuinfo.len > 0 {
		return a.cached_cpuinfo[key]
	}
	info := os.execute('cat /proc/cpuinfo')
	if info.exit_code != 0 {
		return '`cat /proc/cpuinfo` could not run'
	}
	a.cached_cpuinfo = a.parse(info.output, ':')
	return a.cached_cpuinfo[key]
}

fn (mut a App) git_info() string {
	mut out := a.cmd(command: 'git -C . describe --abbrev=8 --dirty --always --tags').trim_space()
	os.execute('git -C . remote add V_REPO https://github.com/vlang/v') // ignore failure (i.e. remote exists)
	os.execute('git -C . fetch V_REPO')
	commit_count := a.cmd(command: 'git rev-list @{0}...V_REPO/master --right-only --count').int()
	if commit_count > 0 {
		out += ' (${commit_count} commit(s) behind V master)'
	}
	return out
}

fn (mut a App) report_tcc_version(tccfolder string) {
	if !os.is_file(os.join_path(tccfolder, '.git', 'config')) {
		a.line(tccfolder, 'N/A')
		return
	}
	tcc_branch_name := a.cmd(
		command: 'git -C ${os.quoted_path(tccfolder)} rev-parse --abbrev-ref HEAD'
	)
	tcc_commit := a.cmd(
		command: 'git -C ${os.quoted_path(tccfolder)} describe --abbrev=8 --dirty --always --tags'
	)
	a.line('${tccfolder} status', '${tcc_branch_name} ${tcc_commit}')
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

fn main() {
	mut app := App{}
	app.collect_info()
	app.report_info()
}
