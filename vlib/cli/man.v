module cli

import time

fn man_flag() Flag {
	return Flag{
		flag: .bool
		name: 'man'
		description: 'Prints the auto-generated manpage.'
	}
}

fn man_cmd() Command {
	return Command{
		name: 'man'
		usage: '<subcommand>'
		description: 'Prints the auto-generated manpage.'
		execute: print_manpage_for_command
	}
}

// print_manpage_for_command prints the manpage for the
// command or subcommand in `man_cmd` to stdout
pub fn print_manpage_for_command(man_cmd Command) ? {
	if man_cmd.args.len > 0 {
		mut cmd := man_cmd.parent
		for arg in man_cmd.args {
			mut found := false
			for sub_cmd in cmd.commands {
				if sub_cmd.name == arg {
					cmd = unsafe { &sub_cmd }
					found = true
					break
				}
			}
			if !found {
				args := man_cmd.args.join(' ')
				println('Invalid command: $args')
				return
			}
		}
		print(cmd.manpage())
	} else {
		if unsafe { man_cmd.parent != 0 } {
			print(man_cmd.parent.manpage())
		}
	}
}

// manpage returns a `string` containing the mdoc(7) manpage for
// this `Command`
pub fn (cmd Command) manpage() string {
	mut mdoc := '.Dd ${time.now().strftime('%B %d, %Y')}\n'
	mdoc += '.Dt ${cmd.full_name().replace(' ', '-').to_upper()} 1\n'
	mdoc += '.Os\n.Sh NAME\n.Nm ${cmd.full_name().replace(' ', '-')}\n.Nd $cmd.description\n'
	mdoc += '.Sh SYNOPSIS\n'
	mdoc += '.Nm $cmd.root().name\n'
	if unsafe { cmd.parent != 0 } {
		mut parents := []Command{}
		if !cmd.parent.is_root() {
			parents.prepend(cmd.parent)
			for {
				p := parents[0]
				if p.parent.is_root() {
					break
				} else {
					parents.prepend(p.parent)
				}
			}
			for c in parents {
				mdoc += '.Ar $c.name\n'
			}
		}
		mdoc += '.Ar $cmd.name\n'
	}
	for flag in cmd.flags {
		mdoc += '.Op'
		if flag.abbrev != '' {
			mdoc += ' Fl $flag.abbrev'
		} else {
			if cmd.posix_mode {
				mdoc += ' Fl -$flag.name'
			} else {
				mdoc += ' Fl $flag.name'
			}
		}
		match flag.flag {
			.int, .float, .int_array, .float_array { mdoc += ' Ar num' }
			.string, .string_array { mdoc += ' Ar string' }
			else {}
		}
		mdoc += '\n'
	}
	for i in 0 .. cmd.required_args {
		mdoc += '.Ar arg$i\n'
	}
	if cmd.commands.len > 0 {
		mdoc += '.Nm $cmd.root().name\n'
		if unsafe { cmd.parent != 0 } {
			mut parents := []Command{}
			if !cmd.parent.is_root() {
				parents.prepend(cmd.parent)
				for {
					p := parents[0]
					if p.parent.is_root() {
						break
					} else {
						parents.prepend(p.parent)
					}
				}
				for c in parents {
					mdoc += '.Ar $c.name\n'
				}
			}
			mdoc += '.Ar $cmd.name\n'
		}
		mdoc += '.Ar subcommand\n'
	}

	mdoc += '.Sh DESCRIPTION\n'
	if cmd.man_description != '' {
		mdoc += '$cmd.man_description\n'
	} else if cmd.description != '' {
		mdoc += '$cmd.description\n'
	}
	if cmd.flags.len > 0 {
		mdoc += '.Pp\nThe options are as follows:\n'
		mdoc += '.Bl -tag -width indent\n'
		for flag in cmd.flags {
			mdoc += '.It'
			if flag.abbrev != '' {
				mdoc += ' Fl $flag.abbrev'
			}
			if cmd.posix_mode {
				mdoc += ' Fl -$flag.name'
			} else {
				mdoc += ' Fl $flag.name'
			}
			mdoc += '\n'
			if flag.description != '' {
				mdoc += '$flag.description\n'
			}
		}
		mdoc += '.El\n'
	}
	if cmd.commands.len > 0 {
		mdoc += '.Pp\nThe subcommands are as follows:\n'
		mdoc += '.Bl -tag -width indent\n'
		for c in cmd.commands {
			mdoc += '.It Cm $c.name\n'
			if c.description != '' {
				mdoc += '$c.description\n'
			}
		}
		mdoc += '.El\n'
	}

	if cmd.commands.len > 0 {
		mdoc += '.Sh SEE ALSO\n'
		mut cmds := []string{}
		if unsafe { cmd.parent != 0 } {
			cmds << cmd.parent.full_name().replace(' ', '-')
		}
		for c in cmd.commands {
			cmds << c.full_name().replace(' ', '-')
		}
		cmds.sort()
		mut i := 1
		for c in cmds {
			mdoc += '.Xr $c 1'
			if i == cmds.len {
				mdoc += '\n'
			} else {
				mdoc += ' ,\n'
			}
			i++
		}
	}

	return mdoc
}
