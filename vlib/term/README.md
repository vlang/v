# Quickstart

The V `term` module is a module which is made to provide an interactive api that helps building TUI apps.

# Use

You can use the `term` module to either color the output on a terminal or to decide on where to put the output in your terminal.

For example let's make a simple program which prints colored text in the middle of the terminal.

```v
import term
import os

fn main() {
        term.clear() // will clear everything and leave a blank terminal
        width, height := term.get_terminal_size() // get the size of the terminal 
        term.set_cursor_position(width / 2, height / 2) // Now we point the cursor to the middle of  the terminal 
        println(term.strikethrough(term.bright_green("hello world")))  // Print green text
        term.set_cursor_position(0, height) // Sets the position of the cursor to the bottom of the terminal
        mut var := os.input('press q to quit: ')
        // KEEP prompting the user until he enters the q key
        for {
                if 'q' in var {
                        break
                } else {
                        var = os.input('press q to quit: ')
                }
        }
}



```

This simple program covers many of the principal aspects of the `term ` module.
