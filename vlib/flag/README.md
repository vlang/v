module flag for command-line flag parsing

- parsing flags like '--flag' or '--stuff=things' or '--things stuff'
- handles bool, int, float and string args
- is able to print usage
- handled unknown arguments as error

Usage example:

 ```v
 module main

 import os
 import flag

 fn main() {
 	mut fp := flag.new_flag_parser(os.args)
 	fp.application('flag_example_tool')
 	fp.version('v0.0.0')
 	fp.description('This tool is only designed to show how the flag lib is working')

 	fp.skip_executable()

 	an_int := fp.int('an_int', 0, 0o666, 'some int to define 0o666 is default')
 	a_bool := fp.bool('a_bool', 0, false, 'some \'real\' flag')
 	a_float := fp.float('a_float', 0, 1.0, 'also floats')
 	a_string := fp.string('a_string', `a`, 'no text', 'finally, some text with "a" an abbreviation')

 	additional_args := fp.finalize() or {
 		eprintln(err)
 		println(fp.usage())
 		return
 	}

 	println('
 		  an_int: $an_int
 		  a_bool: $a_bool
 		 a_float: $a_float
 		a_string: \'$a_string\'
 	')
 	println(additional_args.join_lines())
 }
 ```