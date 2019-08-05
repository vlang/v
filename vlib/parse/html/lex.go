// Package html is an HTML5 lexer following the specifications at http://www.w3.org/TR/html5/syntax.html.
package html

import (
	"io"
	"strconv"

	"github.com/tdewolff/parse/v2"
	"github.com/tdewolff/parse/v2/buffer"
)

// TokenType determines the type of token, eg. a number or a semicolon.
type TokenType uint32

// TokenType values.
const (
	ErrorToken TokenType = iota // extra token when errors occur
	CommentToken
	DoctypeToken
	StartTagToken
	StartTagCloseToken
	StartTagVoidToken
	EndTagToken
	AttributeToken
	TextToken
	SvgToken
	MathToken
)

// String returns the string representation of a TokenType.
func (tt TokenType) String() string {
	switch tt {
	case ErrorToken:
		return "Error"
	case CommentToken:
		return "Comment"
	case DoctypeToken:
		return "Doctype"
	case StartTagToken:
		return "StartTag"
	case StartTagCloseToken:
		return "StartTagClose"
	case StartTagVoidToken:
		return "StartTagVoid"
	case EndTagToken:
		return "EndTag"
	case AttributeToken:
		return "Attribute"
	case TextToken:
		return "Text"
	case SvgToken:
		return "Svg"
	case MathToken:
		return "Math"
	}
	return "Invalid(" + strconv.Itoa(int(tt)) + ")"
}

////////////////////////////////////////////////////////////////

// Lexer is the state for the lexer.
type Lexer struct {
	r   *buffer.Lexer
	err error

	rawTag Hash
	inTag  bool

	text    []byte
	attrVal []byte
}

// NewLexer returns a new Lexer for a given io.Reader.
func NewLexer(r io.Reader) *Lexer {
	return &Lexer{
		r: buffer.NewLexer(r),
	}
}

// Err returns the error encountered during lexing, this is often io.EOF but also other errors can be returned.
func (l *Lexer) Err() error {
	if l.err != nil {
		return l.err
	}
	return l.r.Err()
}

// Restore restores the NULL byte at the end of the buffer.
func (l *Lexer) Restore() {
	l.r.Restore()
}

// Next returns the next Token. It returns ErrorToken when an error was encountered. Using Err() one can retrieve the error message.
func (l *Lexer) Next() (TokenType, []byte) {
	l.text = nil
	var c byte
	if l.inTag {
		l.attrVal = nil
		for { // before attribute name state
			if c = l.r.Peek(0); c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' {
				l.r.Move(1)
				continue
			}
			break
		}
		if c == 0 && l.r.Err() != nil {
			return ErrorToken, nil
		} else if c != '>' && (c != '/' || l.r.Peek(1) != '>') {
			return AttributeToken, l.shiftAttribute()
		}
		start := l.r.Pos()
		l.inTag = false
		if c == '/' {
			l.r.Move(2)
			l.text = l.r.Lexeme()[start:]
			return StartTagVoidToken, l.r.Shift()
		}
		l.r.Move(1)
		l.text = l.r.Lexeme()[start:]
		return StartTagCloseToken, l.r.Shift()
	}

	if l.rawTag != 0 {
		if rawText := l.shiftRawText(); len(rawText) > 0 {
			l.rawTag = 0
			return TextToken, rawText
		}
		l.rawTag = 0
	}

	for {
		c = l.r.Peek(0)
		if c == '<' {
			c = l.r.Peek(1)
			isEndTag := c == '/' && l.r.Peek(2) != '>' && (l.r.Peek(2) != 0 || l.r.PeekErr(2) == nil)
			if l.r.Pos() > 0 {
				if isEndTag || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '!' || c == '?' {
					// return currently buffered texttoken so that we can return tag next iteration
					return TextToken, l.r.Shift()
				}
			} else if isEndTag {
				l.r.Move(2)
				// only endtags that are not followed by > or EOF arrive here
				if c = l.r.Peek(0); !('a' <= c && c <= 'z' || 'A' <= c && c <= 'Z') {
					return CommentToken, l.shiftBogusComment()
				}
				return EndTagToken, l.shiftEndTag()
			} else if 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' {
				l.r.Move(1)
				l.inTag = true
				return l.shiftStartTag()
			} else if c == '!' {
				l.r.Move(2)
				return l.readMarkup()
			} else if c == '?' {
				l.r.Move(1)
				return CommentToken, l.shiftBogusComment()
			}
		} else if c == 0 && l.r.Err() != nil {
			if l.r.Pos() > 0 {
				return TextToken, l.r.Shift()
			}
			return ErrorToken, nil
		}
		l.r.Move(1)
	}
}

// Text returns the textual representation of a token. This excludes delimiters and additional leading/trailing characters.
func (l *Lexer) Text() []byte {
	return l.text
}

// AttrVal returns the attribute value when an AttributeToken was returned from Next.
func (l *Lexer) AttrVal() []byte {
	return l.attrVal
}

////////////////////////////////////////////////////////////////

// The following functions follow the specifications at http://www.w3.org/html/wg/drafts/html/master/syntax.html

func (l *Lexer) shiftRawText() []byte {
	if l.rawTag == Plaintext {
		for {
			if l.r.Peek(0) == 0 && l.r.Err() != nil {
				return l.r.Shift()
			}
			l.r.Move(1)
		}
	} else { // RCDATA, RAWTEXT and SCRIPT
		for {
			c := l.r.Peek(0)
			if c == '<' {
				if l.r.Peek(1) == '/' {
					mark := l.r.Pos()
					l.r.Move(2)
					for {
						if c = l.r.Peek(0); !('a' <= c && c <= 'z' || 'A' <= c && c <= 'Z') {
							break
						}
						l.r.Move(1)
					}
					if h := ToHash(parse.ToLower(parse.Copy(l.r.Lexeme()[mark+2:]))); h == l.rawTag { // copy so that ToLower doesn't change the case of the underlying slice
						l.r.Rewind(mark)
						return l.r.Shift()
					}
				} else if l.rawTag == Script && l.r.Peek(1) == '!' && l.r.Peek(2) == '-' && l.r.Peek(3) == '-' {
					l.r.Move(4)
					inScript := false
					for {
						c := l.r.Peek(0)
						if c == '-' && l.r.Peek(1) == '-' && l.r.Peek(2) == '>' {
							l.r.Move(3)
							break
						} else if c == '<' {
							isEnd := l.r.Peek(1) == '/'
							if isEnd {
								l.r.Move(2)
							} else {
								l.r.Move(1)
							}
							mark := l.r.Pos()
							for {
								if c = l.r.Peek(0); !('a' <= c && c <= 'z' || 'A' <= c && c <= 'Z') {
									break
								}
								l.r.Move(1)
							}
							if h := ToHash(parse.ToLower(parse.Copy(l.r.Lexeme()[mark:]))); h == Script { // copy so that ToLower doesn't change the case of the underlying slice
								if !isEnd {
									inScript = true
								} else {
									if !inScript {
										l.r.Rewind(mark - 2)
										return l.r.Shift()
									}
									inScript = false
								}
							}
						} else if c == 0 && l.r.Err() != nil {
							return l.r.Shift()
						} else {
							l.r.Move(1)
						}
					}
				} else {
					l.r.Move(1)
				}
			} else if c == 0 && l.r.Err() != nil {
				return l.r.Shift()
			} else {
				l.r.Move(1)
			}
		}
	}
}

func (l *Lexer) readMarkup() (TokenType, []byte) {
	if l.at('-', '-') {
		l.r.Move(2)
		for {
			if l.r.Peek(0) == 0 && l.r.Err() != nil {
				return CommentToken, l.r.Shift()
			} else if l.at('-', '-', '>') {
				l.text = l.r.Lexeme()[4:]
				l.r.Move(3)
				return CommentToken, l.r.Shift()
			} else if l.at('-', '-', '!', '>') {
				l.text = l.r.Lexeme()[4:]
				l.r.Move(4)
				return CommentToken, l.r.Shift()
			}
			l.r.Move(1)
		}
	} else if l.at('[', 'C', 'D', 'A', 'T', 'A', '[') {
		l.r.Move(7)
		for {
			if l.r.Peek(0) == 0 && l.r.Err() != nil {
				return TextToken, l.r.Shift()
			} else if l.at(']', ']', '>') {
				l.r.Move(3)
				return TextToken, l.r.Shift()
			}
			l.r.Move(1)
		}
	} else {
		if l.atCaseInsensitive('d', 'o', 'c', 't', 'y', 'p', 'e') {
			l.r.Move(7)
			if l.r.Peek(0) == ' ' {
				l.r.Move(1)
			}
			for {
				if c := l.r.Peek(0); c == '>' || c == 0 && l.r.Err() != nil {
					l.text = l.r.Lexeme()[9:]
					if c == '>' {
						l.r.Move(1)
					}
					return DoctypeToken, l.r.Shift()
				}
				l.r.Move(1)
			}
		}
	}
	return CommentToken, l.shiftBogusComment()
}

func (l *Lexer) shiftBogusComment() []byte {
	for {
		c := l.r.Peek(0)
		if c == '>' {
			l.text = l.r.Lexeme()[2:]
			l.r.Move(1)
			return l.r.Shift()
		} else if c == 0 && l.r.Err() != nil {
			l.text = l.r.Lexeme()[2:]
			return l.r.Shift()
		}
		l.r.Move(1)
	}
}

func (l *Lexer) shiftStartTag() (TokenType, []byte) {
	for {
		if c := l.r.Peek(0); c == ' ' || c == '>' || c == '/' && l.r.Peek(1) == '>' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 0 && l.r.Err() != nil {
			break
		}
		l.r.Move(1)
	}
	l.text = parse.ToLower(l.r.Lexeme()[1:])
	if h := ToHash(l.text); h == Textarea || h == Title || h == Style || h == Xmp || h == Iframe || h == Script || h == Plaintext || h == Svg || h == Math {
		if h == Svg || h == Math {
			data := l.shiftXml(h)
			if l.err != nil {
				return ErrorToken, nil
			}

			l.inTag = false
			if h == Svg {
				return SvgToken, data
			} else {
				return MathToken, data
			}
		}
		l.rawTag = h
	}
	return StartTagToken, l.r.Shift()
}

func (l *Lexer) shiftAttribute() []byte {
	nameStart := l.r.Pos()
	var c byte
	for { // attribute name state
		if c = l.r.Peek(0); c == ' ' || c == '=' || c == '>' || c == '/' && l.r.Peek(1) == '>' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 0 && l.r.Err() != nil {
			break
		}
		l.r.Move(1)
	}
	nameEnd := l.r.Pos()
	for { // after attribute name state
		if c = l.r.Peek(0); c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' {
			l.r.Move(1)
			continue
		}
		break
	}
	if c == '=' {
		l.r.Move(1)
		for { // before attribute value state
			if c = l.r.Peek(0); c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' {
				l.r.Move(1)
				continue
			}
			break
		}
		attrPos := l.r.Pos()
		delim := c
		if delim == '"' || delim == '\'' { // attribute value single- and double-quoted state
			l.r.Move(1)
			for {
				c := l.r.Peek(0)
				if c == delim {
					l.r.Move(1)
					break
				} else if c == 0 && l.r.Err() != nil {
					break
				}
				l.r.Move(1)
			}
		} else { // attribute value unquoted state
			for {
				if c := l.r.Peek(0); c == ' ' || c == '>' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 0 && l.r.Err() != nil {
					break
				}
				l.r.Move(1)
			}
		}
		l.attrVal = l.r.Lexeme()[attrPos:]
	} else {
		l.r.Rewind(nameEnd)
		l.attrVal = nil
	}
	l.text = parse.ToLower(l.r.Lexeme()[nameStart:nameEnd])
	return l.r.Shift()
}

func (l *Lexer) shiftEndTag() []byte {
	for {
		c := l.r.Peek(0)
		if c == '>' {
			l.text = l.r.Lexeme()[2:]
			l.r.Move(1)
			break
		} else if c == 0 && l.r.Err() != nil {
			l.text = l.r.Lexeme()[2:]
			break
		}
		l.r.Move(1)
	}

	end := len(l.text)
	for end > 0 {
		if c := l.text[end-1]; c == ' ' || c == '\t' || c == '\n' || c == '\r' {
			end--
			continue
		}
		break
	}
	l.text = l.text[:end]
	return parse.ToLower(l.r.Shift())
}

// shiftXml parses the content of a svg or math tag according to the XML 1.1 specifications, including the tag itself.
// So far we have already parsed `<svg` or `<math`.
func (l *Lexer) shiftXml(rawTag Hash) []byte {
	inQuote := false
	for {
		c := l.r.Peek(0)
		if c == '"' {
			inQuote = !inQuote
			l.r.Move(1)
		} else if c == '<' && !inQuote && l.r.Peek(1) == '/' {
			mark := l.r.Pos()
			l.r.Move(2)
			for {
				if c = l.r.Peek(0); !('a' <= c && c <= 'z' || 'A' <= c && c <= 'Z') {
					break
				}
				l.r.Move(1)
			}
			if h := ToHash(parse.ToLower(parse.Copy(l.r.Lexeme()[mark+2:]))); h == rawTag { // copy so that ToLower doesn't change the case of the underlying slice
				break
			}
		} else if c == 0 {
			if l.r.Err() == nil {
				l.err = parse.NewErrorLexer("unexpected null character", l.r)
			}
			return l.r.Shift()
		} else {
			l.r.Move(1)
		}
	}

	for {
		c := l.r.Peek(0)
		if c == '>' {
			l.r.Move(1)
			break
		} else if c == 0 {
			if l.r.Err() == nil {
				l.err = parse.NewErrorLexer("unexpected null character", l.r)
			}
			return l.r.Shift()
		}
		l.r.Move(1)
	}
	return l.r.Shift()
}

////////////////////////////////////////////////////////////////

func (l *Lexer) at(b ...byte) bool {
	for i, c := range b {
		if l.r.Peek(i) != c {
			return false
		}
	}
	return true
}

func (l *Lexer) atCaseInsensitive(b ...byte) bool {
	for i, c := range b {
		if l.r.Peek(i) != c && (l.r.Peek(i)+('a'-'A')) != c {
			return false
		}
	}
	return true
}
