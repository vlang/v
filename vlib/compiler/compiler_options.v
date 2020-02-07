// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

pub fn get_v_options_and_main_command(args []string) ([]string,string) {
	mut options := []string
	mut potential_commands := []string
	for i := 0; i < args.len; i++ {
		a := args[i]
		if !a.starts_with('-') {
			potential_commands << a
			continue
		}
		else {
			options << a
			if a in ['-o', '-os', '-cc', '-cflags', '-d'] {
				i++
			}
		}
	}
	// potential_commands[0] is always the executable itself, so ignore it
	command := if potential_commands.len > 1 { potential_commands[1] } else { '' }
	return options,command
}

