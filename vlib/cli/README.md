## Description

`cli` is a command line option parser, that supports
declarative subcommands, each having a separate set of options.

See also the `flag` module, for a simpler command line option parser,
that supports only options.

## Example

```v
module main

import os
import cli

fn main() {
	mut app := cli.Command{
		name:        'example-app'
		description: 'example-app'
		execute:     fn (cmd cli.Command) ! {
			println('hello app')
			return
		}
		commands:    [
			cli.Command{
				name:    'sub'
				alias:   's'
				execute: fn (cmd cli.Command) ! {
					println('hello subcommand')
					return
				}
			},
		]
	}
	app.setup()
	app.parse(os.args)
}
```

Subcommands can set `alias` to accept a shorter invocation token, for example
`example-app s`.

## Help layout

`--help` is generated automatically. Beyond `Usage:`, `Flags:` and `Commands:`
the output picks up extra sections when the relevant `Command` fields are
populated:

- **`Inherited flags:`** — flags inherited from any ancestor through
  `Flag.global`. Renders separately from local flags so a sub-command's
  own surface stays readable.
- **Grouped commands** — set `group` on a sub-command to lift it out of
  the default `Commands:` block into a section named after the group.
  The group string is rendered verbatim followed by `:`, so capitalisation
  is the caller's choice. Sub-commands sharing a group keep their
  declaration order.
- **`Examples:`** — set `examples []string` on a `Command`; each entry
  becomes one indented line.
- **`Learn more:`** — set `learn_more string`; newlines split the block
  into separate lines.

Sections that have nothing to render are simply omitted, so existing apps
see no change unless they opt in by setting these fields. See
`examples/cli_groups.v` for a runnable program that exercises all of them.
