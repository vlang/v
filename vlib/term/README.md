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

# API

Here are some functions you should be aware of in the `term `module:

```v
// returns the height and the width of the terminal
width,height := term.get_terminal_size()


// returns the string as green text to be printed on stdout 
term.ok_message(string)

// returns the string as red text to be printed on stdout 
term.fail_message(string)

// returns the string as yellow text to be printed on stdout 
term.warning_message(string)

//clears the entire terminal and leaves a blank one
term.clear()

// colors the output following the given color, replace <color> with a color of your choice
term.<color>(string)

// Transforms the given string into bold text
term.bold(string)

// Puts a strikethrough into the given string
term.strikethrough(string)

// Underlines the given string
term.underline(string)

// colors the background of the given string, replace <color> witha color of your choice
term.bg_<color>(string)

// Sets the position of the cursor at a given place in the terminal
term.set_cursor_position(x,y)

// moves the cursor up
term.cursor_up()

// moves the cursor down
term.cursor_down()

// moves the cursor to the right
term.cursor_forward()

// moves the cursor to the left
term.cursor_back()

// show the cursor
term.show_cursor()

// hide the cursor
term.hide_cursor


```


