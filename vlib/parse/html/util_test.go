package html

import (
	"testing"

	"github.com/tdewolff/test"
)

func TestEscapeAttrVal(t *testing.T) {
	var escapeAttrValTests = []struct {
		attrVal  string
		expected string
	}{
		{`xyz`, `xyz`},
		{``, ``},
		{`x&amp;z`, `x&amp;z`},
		{`x/z`, `x/z`},
		{`x'z`, `"x'z"`},
		{`x"z`, `'x"z'`},
		{`'x"z'`, `'x"z'`},
		{`'x&#39;"&#39;z'`, `"x'&#34;'z"`},
		{`"x&#34;'&#34;z"`, `'x"&#39;"z'`},
		{`"x&#x27;z"`, `"x'z"`},
		{`'x&#x00022;z'`, `'x"z'`},
		{`'x"&gt;'`, `'x"&gt;'`},
		{`You&#039;re encouraged to log in; however, it&#039;s not mandatory. [o]`, `"You're encouraged to log in; however, it's not mandatory. [o]"`},
		{`a'b=""`, `'a&#39;b=""'`},
		{`x<z`, `"x<z"`},
		{`'x"&#39;"z'`, `'x"&#39;"z'`},
	}
	var buf []byte
	for _, tt := range escapeAttrValTests {
		t.Run(tt.attrVal, func(t *testing.T) {
			b := []byte(tt.attrVal)
			orig := b
			if len(b) > 1 && (b[0] == '"' || b[0] == '\'') && b[0] == b[len(b)-1] {
				b = b[1 : len(b)-1]
			}
			val := EscapeAttrVal(&buf, orig, []byte(b), false)
			test.String(t, string(val), tt.expected)
		})
	}
}

func TestEscapeAttrValXML(t *testing.T) {
	var escapeAttrValTests = []struct {
		attrVal  string
		expected string
	}{
		{`xyz`, `"xyz"`},
		{``, `""`},
	}
	var buf []byte
	for _, tt := range escapeAttrValTests {
		t.Run(tt.attrVal, func(t *testing.T) {
			b := []byte(tt.attrVal)
			orig := b
			if len(b) > 1 && (b[0] == '"' || b[0] == '\'') && b[0] == b[len(b)-1] {
				b = b[1 : len(b)-1]
			}
			val := EscapeAttrVal(&buf, orig, []byte(b), true)
			test.String(t, string(val), tt.expected)
		})
	}
}
