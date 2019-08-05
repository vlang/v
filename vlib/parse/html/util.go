package html

import "github.com/tdewolff/parse/v2"

var (
	singleQuoteEntityBytes = []byte("&#39;")
	doubleQuoteEntityBytes = []byte("&#34;")
)

var charTable = [256]bool{
	// ASCII
	false, false, false, false, false, false, false, false,
	false, true, true, true, true, true, false, false, // tab, new line, vertical tab, form feed, carriage return
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,

	true, false, true, false, false, false, true, true, // space, ", &, '
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, true, true, true, false, // <, =, >

	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,

	true, false, false, false, false, false, false, false, // `
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,

	// non-ASCII
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,

	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,

	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,

	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
	false, false, false, false, false, false, false, false,
}

// EscapeAttrVal returns the escaped attribute value bytes without quotes.
func EscapeAttrVal(buf *[]byte, orig, b []byte, isXML bool) []byte {
	singles := 0
	doubles := 0
	unquoted := true
	entities := false
	for i, c := range b {
		if charTable[c] {
			if c == '&' {
				entities = true
				if quote, n := parse.QuoteEntity(b[i:]); n > 0 {
					if quote == '"' {
						unquoted = false
						doubles++
					} else {
						unquoted = false
						singles++
					}
				}
			} else {
				unquoted = false
				if c == '"' {
					doubles++
				} else if c == '\'' {
					singles++
				}
			}
		}
	}
	if unquoted && !isXML {
		return b
	} else if !entities && len(orig) == len(b)+2 && (singles == 0 && orig[0] == '\'' || doubles == 0 && orig[0] == '"') {
		return orig
	}

	n := len(b) + 2
	var quote byte
	var escapedQuote []byte
	if singles >= doubles || isXML {
		n += doubles * 4
		quote = '"'
		escapedQuote = doubleQuoteEntityBytes
	} else {
		n += singles * 4
		quote = '\''
		escapedQuote = singleQuoteEntityBytes
	}
	if n > cap(*buf) {
		*buf = make([]byte, 0, n) // maximum size, not actual size
	}
	t := (*buf)[:n] // maximum size, not actual size
	t[0] = quote
	j := 1
	start := 0
	for i, c := range b {
		if c == '&' {
			if entityQuote, n := parse.QuoteEntity(b[i:]); n > 0 {
				j += copy(t[j:], b[start:i])
				if entityQuote != quote {
					t[j] = entityQuote
					j++
				} else {
					j += copy(t[j:], escapedQuote)
				}
				start = i + n
			}
		} else if c == quote {
			j += copy(t[j:], b[start:i])
			j += copy(t[j:], escapedQuote)
			start = i + 1
		}
	}
	j += copy(t[j:], b[start:])
	t[j] = quote
	return t[:j+1]
}
