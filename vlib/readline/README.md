# Description

The `readline` module lets you await and read user input
from a terminal in an easy and structured manner.

The module provides an easy way to prompt the user for
questions or even make a REPL or an embedded console.

On Unix, `read_char()` returns a `failed to read character` error when input is unavailable.

Unix line input accepts `ÿ` (U+00FF) as a character. Ctrl+W deletes the word to the left of the
cursor, including on one-character lines, and has no effect on an empty line.

# Usage:

```v
import readline

mut r := readline.Readline{}
answer := r.read_line('hello: ')!
println(answer)
```

or just:

```v
import readline { read_line }

input := read_line('What is your name: ')!
println('Your name is: ${input}')
```
