The `flag` module helps command-line flag parsing.
Main features are:
- parses flags like `-f` or '--flag' or '--stuff=things' or '--things stuff'.
- handles bool, int, float and string args.
- can print usage information listing all the declrared flags.
- handles unknown arguments as error.

Usage example:

```v
module main

import os
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('flag_example_tool')
	fp.version('v0.0.1')
	fp.limit_free_args(0, 0) // comment this, if you expect arbitrary texts after the options
	fp.description('This tool is only designed to show how the flag lib is working')
	fp.skip_executable()
	an_int := fp.int('an_int', 0, 0o123, 'some int to define 0o123 is its default value')
	a_bool := fp.bool('a_bool', 0, false, 'some boolean flag. --a_bool will set it to true.')
	a_float := fp.float('a_float', 0, 1.0, 'some floating point value, by default 1.0 .')
	a_string := fp.string('a_string', `a`, 'no text', 'finally, some text with ' +
		' `-a` as an abbreviation, so you can pass --a_string abc or just -a abc')
	additional_args := fp.finalize() or {
		eprintln(err)
		println(fp.usage())
		return
	}
	println('an_int: $an_int | a_bool: $a_bool | a_float: $a_float | a_string: "$a_string" ')
	println(additional_args.join_lines())
}
```
