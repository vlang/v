import os
import term
import time
import v.util
import runtime

fn main(){
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
		arch_details << cmd(command:'sysctl -n machdep.cpu.brand_string')
	}
	if os_kind == 'linux' {
		arch_details << cmd(command:'grep "model name" /proc/cpuinfo | sed "s/.*: //gm"')
	}
	if os_kind == 'windows' {
		arch_details << cmd(command:'wmic cpu get name /format:table', line: 1)
	}
	//
	mut os_details := ''
	if os_kind == 'linux' {
		os_details = get_linux_os_name()
	} else if os_kind == 'mac' {
		mut details := []string{}
		details << cmd(command: 'sw_vers -productName')
		details << cmd(command: 'sw_vers -productVersion')
		details << cmd(command: 'sw_vers -buildVersion')
		os_details = details.join(', ')
	} else if os_kind == 'windows' {
		os_details = cmd(command:'wmic os get name, buildnumber, osarchitecture', line: 1)
	} else {
		ouname := os.uname()
		os_details = '$ouname.release, $ouname.version'
	}
	line('OS', '$os_kind, $os_details')
	line('Processor', arch_details.join(', '))
	line('CC version', cmd(command:'cc --version'))
	println(util.bold(term.h_divider('-')))
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	line('vroot', vroot)
	line('vexe', vexe)
	line('vexe mtime', time.unix(os.file_last_mod_unix(vexe)).str())
	is_writable_vroot := os.is_writable_folder(vroot) or { false }
	line('is vroot writable', is_writable_vroot.str())
	line('V full version', util.full_v_version(true))
	vtmp := os.getenv('VTMP')
	if vtmp != '' {
		line('env VTMP', '"$vtmp"')
	}
	vflags := os.getenv('VFLAGS')
	if vflags != '' {
		line('env VFLAGS', '"$vflags"')
	}
	println(util.bold(term.h_divider('-')))
	line('Git version', cmd(command:'git --version'))
	line('Git vroot status', cmd(command:'git -C . describe --abbrev=8 --dirty --always --tags'))
	line('.git/config present', os.is_file('.git/config').str())
	println(util.bold(term.h_divider('-')))
}

struct CmdConfig {
	line int
	command string
}

fn cmd(c CmdConfig) string {
	x := os.exec(c.command) or {
		return 'N/A'
	}
	if x.exit_code == 0 {
		return x.output.split_into_lines()[c.line]
	}
	return 'Error: $x.output'
}

fn line(label string, value string) {
	println('$label: ${util.bold(value)}')
}

fn get_linux_os_name() string {
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
					vals[x[0]] = x[1].trim('"')
				}
				if vals['PRETTY_NAME'] == '' {
					continue
				}
				os_details = vals['PRETTY_NAME']
				break
			}
			'lsb_release' {
				exists := cmd(command:'type lsb_release')
				if exists.starts_with('Error') {
					continue
				}
				os_details = cmd(command: 'lsb_release -d -s')
				break
			}
			'kernel' {
				if !os.is_file('/proc/version') {
					continue
				}
				os_details = cmd(command: 'cat /proc/version')
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
