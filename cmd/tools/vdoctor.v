import os
import term
import time
import v.util
import runtime

struct App {
mut:
	report_lines []string
}

fn (mut a App) println(s string) {
	a.report_lines << s
}

fn (mut a App) collect_info() {
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
	if os_kind == 'mac' {
		arch_details << a.cmd(command:'sysctl -n machdep.cpu.brand_string')
	}
	if os_kind == 'linux' {
		mname := a.cmd(command:'grep "model name" /proc/cpuinfo | sed "s/.*: //gm"')
		if !mname.starts_with('Error:') {
			arch_details << mname
		} else {
			hinfo := a.cmd(command:'grep "Hardware" /proc/cpuinfo | sed "s/.*: //gm"')
			arch_details << hinfo
		}
	}
	if os_kind == 'windows' {
		arch_details << a.cmd(command:'wmic cpu get name /format:table', line: 1)
	}
	//
	mut os_details := ''
	if os_kind == 'linux' {
		os_details = a.get_linux_os_name()
	} else if os_kind == 'mac' {
		mut details := []string{}
		details << a.cmd(command: 'sw_vers -productName')
		details << a.cmd(command: 'sw_vers -productVersion')
		details << a.cmd(command: 'sw_vers -buildVersion')
		os_details = details.join(', ')
	} else if os_kind == 'windows' {
		os_details = a.cmd(command:'wmic os get name, buildnumber, osarchitecture', line: 1)
	} else {
		ouname := os.uname()
		os_details = '$ouname.release, $ouname.version'
	}
	a.line('OS', '$os_kind, $os_details')
	a.line('Processor', arch_details.join(', '))
	a.line('CC version', a.cmd(command:'cc --version'))
	a.println(util.bold(term.h_divider('-')))
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	a.line('vroot', vroot)
	a.line('vexe', vexe)
	a.line('vexe mtime', time.unix(os.file_last_mod_unix(vexe)).str())
	is_writable_vroot := os.is_writable_folder(vroot) or { false }
	a.line('is vroot writable', is_writable_vroot.str())
	a.line('V full version', util.full_v_version(true))
	vtmp := os.getenv('VTMP')
	if vtmp != '' {
		a.line('env VTMP', '"$vtmp"')
	}
	vflags := os.getenv('VFLAGS')
	if vflags != '' {
		a.line('env VFLAGS', '"$vflags"')
	}
	a.println(util.bold(term.h_divider('-')))
	a.line('Git version', a.cmd(command:'git --version'))
	a.line('Git vroot status', a.cmd(command:'git -C . describe --abbrev=8 --dirty --always --tags'))
	a.line('.git/config present', os.is_file('.git/config').str())
	//
	if os_kind == 'linux' {
		a.report_tcc_version('/var/tmp/tcc')
	}
	a.report_tcc_version('thirdparty/tcc')
	//
	a.println(util.bold(term.h_divider('-')))
}

struct CmdConfig {
	line int
	command string
}

fn (mut a App) cmd(c CmdConfig) string {
	x := os.exec(c.command) or {
		return 'N/A'
	}
	if x.exit_code == 0 {
		output := x.output.split_into_lines()
		if output.len > 0 && output.len > c.line {
			return output[c.line]
		}
	}
	return 'Error: $x.output'
}

fn (mut a App) line(label string, value string) {
	a.println('$label: ${util.bold(value)}')
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
				lines := os.read_lines('/etc/os-release') or {
					continue
				}
				mut vals := map[string]string
				for line in lines {
					x := line.split('=')
					if x.len > 1 {
						vals[x[0]] = x[1].trim('"')
					}
				}
				if vals['PRETTY_NAME'] == '' {
					continue
				}
				os_details = vals['PRETTY_NAME']
				break
			}
			'lsb_release' {
				exists := a.cmd(command:'type lsb_release')
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
				os_details = '$ouname.release, $ouname.version'
				break
			}
			else {}
		}
	}
	return os_details
}

fn (mut a App) report_tcc_version(tccfolder string) {
	if !os.is_file(os.join_path(tccfolder, '.git', 'config')) {
		a.line(tccfolder, 'N/A')
		return
	}
	tcc_branch_name := a.cmd(command:'git -C $tccfolder rev-parse --abbrev-ref HEAD')
	tcc_commit := a.cmd(command:'git -C $tccfolder describe --abbrev=8 --dirty --always --tags')
	a.line('$tccfolder status', '$tcc_branch_name $tcc_commit')
}

fn (mut a App) report_info() {
	for x in a.report_lines {
		println(x)
	}
}

fn main(){
	mut app := App{}
	app.collect_info()
	app.report_info()
}
