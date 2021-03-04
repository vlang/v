module main

import cli
import vls
import v.vmod
import os

fn C._setmode(int, int)

const (
	meta = vmod.decode(@VMOD_FILE) ?
)

fn run_cli(cmd cli.Command) ? {
	mut ls := vls.new(&Stdio{})
	enable_flag_raw := cmd.flags.get_string('enable') or { '' }
	disable_flag_raw := cmd.flags.get_string('disable') or { '' }
	enable_features := if enable_flag_raw.len > 0 { enable_flag_raw.split(',') } else { []string{} }
	disable_features := if disable_flag_raw.len > 0 {
		disable_flag_raw.split(',')
	} else {
		[]string{}
	}
	ls.set_features(enable_features, true) ?
	ls.set_features(disable_features, false) ?
	ls.start_loop()
}

fn main() {
	$if windows {
		// 0x8000 = _O_BINARY from <fcntl.h>
		// windows replaces \n => \r\n, so \r\n will be replaced to \r\r\n
		// binary mode prevents this
		C._setmode(C._fileno(C.stdout), 0x8000)
	}
	mut cmd := cli.Command{
		name: 'vls'
		version: meta.version
		description: meta.description
		execute: run_cli
	}

	cmd.add_flags([
		cli.Flag{
			flag: .string
			name: 'enable'
			abbrev: 'e'
			description: 'Enables specific language features.'
		},
		cli.Flag{
			flag: .string
			name: 'disable'
			abbrev: 'd'
			description: 'Disables specific language features.'
		},
	])

	cmd.parse(os.args)
}
