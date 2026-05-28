## Description

The `term` module is designed to provide the building blocks for building
very simple TUI apps. For more complex apps, you should really look at the
`term.ui` module, as it includes terminal events, is easier to use and
is much more performant for large draws.

## Usage

You can use the `term` module to either color the output on a terminal
or to decide on where to put the output in your terminal.

For example let's make a simple program which prints colored text in the middle of the terminal.

```v
import term
import os

fn main() {
	term.clear() // clears the content in the terminal
	width, height := term.get_terminal_size() // get the size of the terminal
	term.set_cursor_position(x: width / 2, y: height / 2) // now we point the cursor to the middle of  the terminal
	println(term.strikethrough(term.bright_green('hello world'))) // Print green text
	term.set_cursor_position(x: 0, y: height) // Sets the position of the cursor to the bottom of the terminal
	// Keep prompting until the user presses the q key
	for {
		if var := os.input_opt('press q to quit: ') {
			if var != 'q' {
				continue
			}
			break
		}
		println('')
		break
	}
	println('Goodbye.')
}
```

This simple program covers many of the principal aspects of the `term ` module.

## API

Here are some of the main functions of the `term` module. Note that the
coloring/styling functions like `ok_message`, `yellow`, `bold`, etc., do
*not* print anything by themselves — they return a new string with ANSI
escape codes embedded, which you can then pass to `println` (or similar)
to actually display the styled text on stdout.

```v
import term
import os

// returns the height and the width of the terminal
width, height := term.get_terminal_size()
println('terminal dimensions: width: ${width} height: ${height}')

mut output := ''

// returns the string as green text
output = 'ok_message() text ' + term.ok_message('is green')
println(output)

// returns the string as red text
output = 'fail_message() text ' + term.fail_message('is red')
println(output)

// returns the string as yellow text
output = 'warn_message() text ' + term.warn_message('is yellow')
println(output)

os.input('hit Enter to clear the console and continue')

// clears the entire terminal
term.clear()

// Set the color output of the text.
// The available colors are:
// black, white, blue, yellow,
// green, red, cyan, magenta,
// bright_black, bright_white, bright_blue, bright_yellow,
// bright_green, bright_red, bright_cyan, bright_magenta,
output = 'yellow() - ' + term.yellow('text')
println(output)

// transforms the given string into bold text
output = 'bold() - ' + term.bold('text')
println(output)

// puts a strikethrough into the given string
output = 'strikethrough() - ' + term.strikethrough('text')
println(output)

// underlines the given string
output = 'underline() - ' + term.underline('text')
println(output)

// colors the background of the output following the given color
// the available colors are: black, blue, yellow, green, cyan, gray
output = 'bg_green() - ' + term.bg_green('text')
println(output)

// sets the position of the cursor
term.set_cursor_position(x: 5, y: 10)
println('Cursor at (5,10)')

// flashes (blinks) the text
output = term.slow_blink('done')
println(output)
```

The `term` module also provides several lower-level cursor-control
helpers, which write directly to stdout:

```v
import term

// moves the cursor up
term.cursor_up(1)
// moves the cursor down
term.cursor_down(1)
// moves the cursor to the right
term.cursor_forward(2)
// moves the cursor to the left
term.cursor_back(2)
// hides the cursor
term.hide_cursor()
// shows the cursor
term.show_cursor()
```
