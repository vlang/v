Usage example:

```v
module main

import os
import cli

fn main() {
	mut app := cli.Command{
		name: 'example-app'
		description: 'example-app'
		execute: fn (cmd cli.Command) ? {
			println('hello app')
			return
		}
		commands: [
			cli.Command{
				name: 'sub'
				execute: fn (cmd cli.Command) ? {
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
