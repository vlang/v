# os.cmdline

`split(text)!` separates a command string into shell-like words.
Single and double quotes group text; adjacent quoted and unquoted parts form one word.
An empty quoted string produces an empty word. Outside quotes, backslash escapes the next byte,
and backslash followed by newline continues the current word.
Within double quotes, backslash escapes dollar, backtick, double quote, backslash, or newline;
other backslashes remain literal. Single-quoted text is literal.

Unbalanced quotes and a trailing unquoted backslash return errors.
Whitespace separates words. The function does not expand variables, execute substitutions,
or interpret `#` comments. It only tokenizes input;
callers decide how to execute the resulting words.
