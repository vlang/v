package html

import (
	"bytes"
	"fmt"
	"io"
	"testing"

	"github.com/tdewolff/parse/v2"
	"github.com/tdewolff/test"
)

type TTs []TokenType

func TestTokens(t *testing.T) {
	var tokenTests = []struct {
		html     string
		expected []TokenType
	}{
		{"<html></html>", TTs{StartTagToken, StartTagCloseToken, EndTagToken}},
		{"<img/>", TTs{StartTagToken, StartTagVoidToken}},
		{"<!-- comment -->", TTs{CommentToken}},
		{"<!-- comment --!>", TTs{CommentToken}},
		{"<p>text</p>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken}},
		{"<input type='button'/>", TTs{StartTagToken, AttributeToken, StartTagVoidToken}},
		{"<input  type='button'  value=''/>", TTs{StartTagToken, AttributeToken, AttributeToken, StartTagVoidToken}},
		{"<input type='=/>' \r\n\t\f value=\"'\" name=x checked />", TTs{StartTagToken, AttributeToken, AttributeToken, AttributeToken, AttributeToken, StartTagVoidToken}},
		{"<!doctype>", TTs{DoctypeToken}},
		{"<!doctype html>", TTs{DoctypeToken}},
		{"<?bogus>", TTs{CommentToken}},
		{"</0bogus>", TTs{CommentToken}},
		{"<!bogus>", TTs{CommentToken}},
		{"< ", TTs{TextToken}},
		{"</", TTs{TextToken}},

		// raw tags
		{"<title><p></p></title>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken}},
		{"<TITLE><p></p></TITLE>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken}},
		{"<plaintext></plaintext>", TTs{StartTagToken, StartTagCloseToken, TextToken}},
		{"<script></script>", TTs{StartTagToken, StartTagCloseToken, EndTagToken}},
		{"<script>var x='</script>';</script>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken, TextToken, EndTagToken}},
		{"<script><!--var x='</script>';--></script>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken, TextToken, EndTagToken}},
		{"<script><!--var x='<script></script>';--></script>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken}},
		{"<script><!--var x='<script>';--></script>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken}},
		{"<![CDATA[ test ]]>", TTs{TextToken}},
		{"<svg>text</svg>", TTs{SvgToken}},
		{"<math>text</math gibberish>", TTs{MathToken}},
		{`<svg>text<x a="</svg>"></x></svg>`, TTs{SvgToken}},
		{"<a><svg>text</svg></a>", TTs{StartTagToken, StartTagCloseToken, SvgToken, EndTagToken}},

		// early endings
		{"<!-- comment", TTs{CommentToken}},
		{"<? bogus comment", TTs{CommentToken}},
		{"<foo", TTs{StartTagToken}},
		{"</foo", TTs{EndTagToken}},
		{"<foo x", TTs{StartTagToken, AttributeToken}},
		{"<foo x=", TTs{StartTagToken, AttributeToken}},
		{"<foo x='", TTs{StartTagToken, AttributeToken}},
		{"<foo x=''", TTs{StartTagToken, AttributeToken}},
		{"<!DOCTYPE note SYSTEM", TTs{DoctypeToken}},
		{"<![CDATA[ test", TTs{TextToken}},
		{"<script>", TTs{StartTagToken, StartTagCloseToken}},
		{"<script><!--", TTs{StartTagToken, StartTagCloseToken, TextToken}},
		{"<script><!--var x='<script></script>';-->", TTs{StartTagToken, StartTagCloseToken, TextToken}},

		// NULL
		{"foo\x00bar", TTs{TextToken}},
		{"<\x00foo>", TTs{TextToken}},
		{"<foo\x00>", TTs{StartTagToken, StartTagCloseToken}},
		{"</\x00bogus>", TTs{CommentToken}},
		{"</foo\x00>", TTs{EndTagToken}},
		{"<plaintext>\x00</plaintext>", TTs{StartTagToken, StartTagCloseToken, TextToken}},
		{"<script>\x00</script>", TTs{StartTagToken, StartTagCloseToken, TextToken, EndTagToken}},
		{"<!--\x00-->", TTs{CommentToken}},
		{"<![CDATA[\x00]]>", TTs{TextToken}},
		{"<!doctype\x00>", TTs{DoctypeToken}},
		{"<?bogus\x00>", TTs{CommentToken}},
		{"<?bogus\x00>", TTs{CommentToken}},

		// go-fuzz
		{"</>", TTs{TextToken}},
	}
	for _, tt := range tokenTests {
		t.Run(tt.html, func(t *testing.T) {
			l := NewLexer(bytes.NewBufferString(tt.html))
			i := 0
			for {
				token, _ := l.Next()
				if token == ErrorToken {
					test.T(t, l.Err(), io.EOF)
					test.T(t, i, len(tt.expected), "when error occurred we must be at the end")
					break
				}
				test.That(t, i < len(tt.expected), "index", i, "must not exceed expected token types size", len(tt.expected))
				if i < len(tt.expected) {
					test.T(t, token, tt.expected[i], "token types must match")
				}
				i++
			}
		})
	}

	// coverage
	for i := 0; ; i++ {
		if TokenType(i).String() == fmt.Sprintf("Invalid(%d)", i) {
			break
		}
	}
}

func TestTags(t *testing.T) {
	var tagTests = []struct {
		html     string
		expected string
	}{
		{"<foo:bar.qux-norf/>", "foo:bar.qux-norf"},
		{"<foo?bar/qux>", "foo?bar/qux"},
		{"<!DOCTYPE note SYSTEM \"Note.dtd\">", " note SYSTEM \"Note.dtd\""},
		{"</foo >", "foo"},

		// early endings
		{"<foo ", "foo"},
	}
	for _, tt := range tagTests {
		t.Run(tt.html, func(t *testing.T) {
			l := NewLexer(bytes.NewBufferString(tt.html))
			for {
				token, _ := l.Next()
				if token == ErrorToken {
					test.T(t, l.Err(), io.EOF)
					test.Fail(t, "when error occurred we must be at the end")
					break
				} else if token == StartTagToken || token == EndTagToken || token == DoctypeToken {
					test.String(t, string(l.Text()), tt.expected)
					break
				}
			}
		})
	}
}

func TestAttributes(t *testing.T) {
	var attributeTests = []struct {
		attr     string
		expected []string
	}{
		{"<foo a=\"b\" />", []string{"a", "\"b\""}},
		{"<foo \nchecked \r\n value\r=\t'=/>\"' />", []string{"checked", "", "value", "'=/>\"'"}},
		{"<foo bar=\" a \n\t\r b \" />", []string{"bar", "\" a \n\t\r b \""}},
		{"<foo a/>", []string{"a", ""}},
		{"<foo /=/>", []string{"/", "/"}},

		// early endings
		{"<foo x", []string{"x", ""}},
		{"<foo x=", []string{"x", ""}},
		{"<foo x='", []string{"x", "'"}},

		// NULL
		{"<foo \x00>", []string{"\x00", ""}},
		{"<foo \x00=\x00>", []string{"\x00", "\x00"}},
		{"<foo \x00='\x00'>", []string{"\x00", "'\x00'"}},
	}
	for _, tt := range attributeTests {
		t.Run(tt.attr, func(t *testing.T) {
			l := NewLexer(bytes.NewBufferString(tt.attr))
			i := 0
			for {
				token, _ := l.Next()
				if token == ErrorToken {
					test.T(t, l.Err(), io.EOF)
					test.T(t, i, len(tt.expected), "when error occurred we must be at the end")
					break
				} else if token == AttributeToken {
					test.That(t, i+1 < len(tt.expected), "index", i+1, "must not exceed expected attributes size", len(tt.expected))
					if i+1 < len(tt.expected) {
						test.String(t, string(l.Text()), tt.expected[i], "attribute keys must match")
						test.String(t, string(l.AttrVal()), tt.expected[i+1], "attribute keys must match")
						i += 2
					}
				}
			}
		})
	}
}

func TestErrors(t *testing.T) {
	var errorTests = []struct {
		html string
		col  int
	}{
		{"<svg>\x00</svg>", 6},
		{"<svg></svg\x00>", 11},
	}
	for _, tt := range errorTests {
		t.Run(tt.html, func(t *testing.T) {
			l := NewLexer(bytes.NewBufferString(tt.html))
			for {
				token, _ := l.Next()
				if token == ErrorToken {
					if tt.col == 0 {
						test.T(t, l.Err(), io.EOF)
					} else if perr, ok := l.Err().(*parse.Error); ok {
						_, col, _ := perr.Position()
						test.T(t, col, tt.col)
					} else {
						test.Fail(t, "bad error:", l.Err())
					}
					break
				}
			}
		})
	}
}

////////////////////////////////////////////////////////////////

var J int
var ss = [][]byte{
	[]byte(" style"),
	[]byte("style"),
	[]byte(" \r\n\tstyle"),
	[]byte("      style"),
	[]byte(" x"),
	[]byte("x"),
}

func BenchmarkWhitespace1(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, s := range ss {
			j := 0
			for {
				if c := s[j]; c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' {
					j++
				} else {
					break
				}
			}
			J += j
		}
	}
}

func BenchmarkWhitespace2(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, s := range ss {
			j := 0
			for {
				if c := s[j]; c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' {
					j++
					continue
				}
				break
			}
			J += j
		}
	}
}

func BenchmarkWhitespace3(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, s := range ss {
			j := 0
			for {
				if c := s[j]; c != ' ' && c != '\t' && c != '\n' && c != '\r' && c != '\f' {
					break
				}
				j++
			}
			J += j
		}
	}
}

////////////////////////////////////////////////////////////////

func ExampleNewLexer() {
	l := NewLexer(bytes.NewBufferString("<span class='user'>John Doe</span>"))
	out := ""
	for {
		tt, data := l.Next()
		if tt == ErrorToken {
			break
		}
		out += string(data)
	}
	fmt.Println(out)
	// Output: <span class='user'>John Doe</span>
}
