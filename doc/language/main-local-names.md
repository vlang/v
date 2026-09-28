# Local names in the program entry point

The names `argc` and `argv` are available for ordinary V variables, including in
`main` and scripts. The C backend keeps those variables separate from the native
entry point parameters. Command-line arguments remain available through `os.args`.
