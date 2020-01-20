module yaml

struct Parser{
	scanner &scanner.Scanner
mut:
	tok token.token
}