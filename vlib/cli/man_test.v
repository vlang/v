module cli

fn test_manpage() {
	mut cmd := Command{
		name: 'command'
		description: 'description'
		commands: [
			Command{
				name: 'sub'
				description: 'subcommand'
			},
			Command{
				name: 'sub2'
				description: 'another subcommand'
			},
		]
		flags: [
			Flag{
				flag: .string
				name: 'str'
				description: 'str flag'
			},
			Flag{
				flag: .bool
				name: 'bool'
				description: 'bool flag'
				abbrev: 'b'
			},
			Flag{
				flag: .string
				name: 'required'
				abbrev: 'r'
				required: true
			},
		]
	}
	cmd.setup()
	assert cmd.manpage().after_char(`\n`) == r'.Dt COMMAND 1
.Os
.Sh NAME
.Nm command
.Nd description
.Sh SYNOPSIS
.Nm command
.Op Fl str Ar string
.Op Fl b
.Op Fl r Ar string
.Nm command
.Ar subcommand
.Sh DESCRIPTION
description
.Pp
The options are as follows:
.Bl -tag -width indent
.It Fl str
str flag
.It Fl b Fl bool
bool flag
.It Fl r Fl required
.El
.Pp
The subcommands are as follows:
.Bl -tag -width indent
.It Cm sub
subcommand
.It Cm sub2
another subcommand
.El
.Sh SEE ALSO
.Xr command-sub 1 ,
.Xr command-sub2 1
'

	cmd.posix_mode = true
	assert cmd.manpage().after_char(`\n`) == r'.Dt COMMAND 1
.Os
.Sh NAME
.Nm command
.Nd description
.Sh SYNOPSIS
.Nm command
.Op Fl -str Ar string
.Op Fl b
.Op Fl r Ar string
.Nm command
.Ar subcommand
.Sh DESCRIPTION
description
.Pp
The options are as follows:
.Bl -tag -width indent
.It Fl -str
str flag
.It Fl b Fl -bool
bool flag
.It Fl r Fl -required
.El
.Pp
The subcommands are as follows:
.Bl -tag -width indent
.It Cm sub
subcommand
.It Cm sub2
another subcommand
.El
.Sh SEE ALSO
.Xr command-sub 1 ,
.Xr command-sub2 1
'
}
