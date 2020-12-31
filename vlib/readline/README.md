# Readline

The `readline` module let you await and read user input
from a terminal in an easy and structured manner.

The module provides an easy way to prompt the user for
questions or even make a REPL or an embedded console.

Use `readline.Readline` if you want to include more
advanced features such as history or simply use
`readline.read_line('Please confirm (y/n):')` directly
for one-off user interactions.

# Usage

```v ignore
import readline { Readline }

Readline.read_line('Continue?: (y/n)')
```
