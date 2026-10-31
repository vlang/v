# Postfix operators and comments

Postfix values such as `take(counter++)` and `values[counter--]` produce the same warning
when whitespace or comments appear before the closing delimiter, including nested comments.
For example, `take(counter++ /* outer /* inner */ rest */)` warns like `take(counter++)`.

Delimiters inside comments do not close expressions. The value expression
`counter++ /* outer /* inner */ ) */ + 1` follows the usual post-increment assignment rule.
An explicit `-W` turns a postfix-value warning into an error; `-prod` alone keeps it a warning.
