module readline

#const $readline = require('readline')

struct Termios {}

// Only use standard os.get_line
// Need implementation for readline capabilities
//
// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete input line as an UTF-8 encoded `[]rune` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.

pub fn (mut r Readline) read_line(prompt string) !string {
	res := ''
	print(prompt)
	#const rl = $readline.createInterface({input: $process.stdin,output: $process.stdout,prompt: prompt.str})
	#rl.prompt()
	#rl.on('line', function (ans) { rl.prompt(); res.str = ans; rl.close();})

	return res
}

pub fn read_line(prompt string) !string {
	mut r := Readline{}
	s := r.read_line(prompt)!
	return s
}
