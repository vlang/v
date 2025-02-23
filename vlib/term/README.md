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

Here are some of the main functions of the `term `module:

```v
import term

// returns the height and the width of the terminal
width, height := term.get_terminal_size()
println('width: ${width}, height: ${height}')
// returns the string as green text to be printed on stdout
term.ok_message('cool')
// returns the string as red text to be printed on stdout
term.fail_message('oh, no')
// returns the string as yellow text to be printed on stdout
term.warn_message('be warned')
// clears the entire terminal and leaves a blank one
term.clear()

// Set the color output of the output.
// The available colors are:
// black, white, blue, yellow,
// green, red, cyan, magenta,
// bright_black, bright_white, bright_blue, bright_yellow,
// bright_green, bright_red, bright_cyan, bright_magenta,
term.yellow('submarine')

// transforms the given string into bold text
term.bold('and beautiful')
// puts a strikethrough into the given string
term.strikethrough('the core of the problem')
// underlines the given string
term.underline('important')
// colors the background of the output following the given color
// the available colors are: black, blue, yellow, green, cyan, gray
term.bg_green('field')
// sets the position of the cursor at a given place in the terminal
term.set_cursor_position(x: 5, y: 10)
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
