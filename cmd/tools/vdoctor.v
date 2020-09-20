import os
import term
import time
import v.util
import runtime

fn main(){
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
	line('Processor', arch_details.join(', '))
	mut os_kind := os.user_os()
	mut os_details := ''
	if os_kind == 'linux' {
		os_details = first_line_of_cmd('lsb_release -d -s')
	}
	line('OS', os_kind)
	line('OS details', os_details)
	line('CC version', first_line_of_cmd('cc --version'))
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
	println(util.bold(term.h_divider('-')))
	line('Git version', first_line_of_cmd('git --version'))
	line('Git vroot status', first_line_of_cmd('git describe --dirty --tags'))
	line('.git/config present', os.is_file('.git/config').str())
	println(util.bold(term.h_divider('-')))
}

fn first_line_of_cmd(cmd string) string {
	x := os.exec(cmd) or {
		return 'N/A'
	}
	if x.exit_code == 0 {
		return x.output.split_into_lines()[0]
	}
	println('first_line_of_cmd error: $x.output')
	return 'Error'
}

fn line(label string, value string) {
	println('$label: ${util.bold(value)}')
}
