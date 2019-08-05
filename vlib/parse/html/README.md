# HTML [![GoDoc](http://godoc.org/github.com/tdewolff/parse/html?status.svg)](http://godoc.org/github.com/tdewolff/parse/html)

This package is an HTML5 lexer written in [Go][1]. It follows the specification at [The HTML syntax](http://www.w3.org/TR/html5/syntax.html). The lexer takes an io.Reader and converts it into tokens until the EOF.

## Installation
Run the following command

	go get -u github.com/tdewolff/parse/v2/html

or add the following import and run project with `go get`

	import "github.com/tdewolff/parse/v2/html"

## Lexer
### Usage
The following initializes a new Lexer with io.Reader `r`:
``` go
l := html.NewLexer(r)
```

To tokenize until EOF an error, use:
``` go
for {
	tt, data := l.Next()
	switch tt {
	case html.ErrorToken:
		// error or EOF set in l.Err()
		return
	case html.StartTagToken:
		// ...
		for {
			ttAttr, dataAttr := l.Next()
			if ttAttr != html.AttributeToken {
				break
			}
			// ...
		}
	// ...
	}
}
```

All tokens:
``` go
ErrorToken TokenType = iota // extra token when errors occur
CommentToken
DoctypeToken
StartTagToken
StartTagCloseToken
StartTagVoidToken
EndTagToken
AttributeToken
TextToken
```

### Examples
``` go
package main

import (
	"os"

	"github.com/tdewolff/parse/v2/html"
)

// Tokenize HTML from stdin.
func main() {
	l := html.NewLexer(os.Stdin)
	for {
		tt, data := l.Next()
		switch tt {
		case html.ErrorToken:
			if l.Err() != io.EOF {
				fmt.Println("Error on line", l.Line(), ":", l.Err())
			}
			return
		case html.StartTagToken:
			fmt.Println("Tag", string(data))
			for {
				ttAttr, dataAttr := l.Next()
				if ttAttr != html.AttributeToken {
					break
				}

				key := dataAttr
				val := l.AttrVal()
				fmt.Println("Attribute", string(key), "=", string(val))
			}
		// ...
		}
	}
}
```

## License
Released under the [MIT license](https://github.com/tdewolff/parse/blob/master/LICENSE.md).

[1]: http://golang.org/ "Go Language"
