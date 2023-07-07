module html

/*
unescape() automatically convert HTML Encoded text to its original text
and it doesn't matter the HTML Encoded be in which type (symbolic, decimal, hex)
all the types are supported

Usage:

```
import html

fn main() {
	text := "Hello,&nbsp;&apos;World&apos;"

	decode := html.unescape(text)

	println(decode)
}
```
*/
pub fn unescape(input string) string {
	// For adding more HTML Encode characters just add them to the lists below

	// Symbolic HTML Encode
	mut symbol := ['&amp;', '&', '&lt;', '<', '&gt;', '>', '&apos;', "'", '&quot;', '"', '&nbsp;',
		' ']
	// Decimal HTML Encode
	mut decimal := ['&#38;', '&', '&#60;', '<', '&#62;', '>', '&#39;', "'", '&#34;', '"', '&#160;',
		' ']
	// Hexadecimal HTML Encode
	mut hex := ['&#x26;', '&', '&#x3C;', '<', '&#x3E;', '>', '&#x27;', "'", '&#x22;', '"', '&#xA0;',
		' ']

	is_contains_decimal := [decimal[0], decimal[2], decimal[4], decimal[6], decimal[8], decimal[10]]
	is_contains_hex := [hex[0], hex[2], hex[4], hex[6], hex[8], hex[10]]

	mut each_decimal := ''
	for i in is_contains_decimal {
		each_decimal = i
	}
	mut each_hex := ''
	for i in is_contains_hex {
		each_hex = i
	}
	return if input.contains(each_decimal) {
		input.replace_each(decimal)
	} else if input.contains(each_hex) {
		input.replace_each(hex)
	} else {
		input.replace_each(symbol)
	}
}
