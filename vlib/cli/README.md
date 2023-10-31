## Description:

`cli` is a command line option parser, that supports
declarative subcommands, each having separate set of options.

See also the `flag` module, for a simpler command line option parser,
that supports only options.

## Example:

```v
module main

import os
import cli

fn main() {
	mut app := cli.Command{
		name: 'example-app'
		description: 'example-app'
		execute: fn (cmd cli.Command) ! {
			println('hello app')
			return
		}
		commands: [
			cli.Command{
				name: 'sub'
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
