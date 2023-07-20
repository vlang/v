module html

/*
unescape() automatically convert HTML Encoded text to its original text
and it doesn't matter the HTML Encoded be in which type (symbolic, decimal, hex)
all the types are supported
Usage:
```
import html
fn main() {
	text := "Hello, &apos;World&apos;"
	decode := html.unescape(text)
	println(decode)
}
```
*/
pub fn unescape(input string) string {
	mut escape_to_text := ['&amp;', '&', '&lt;', '<', '&gt;', '>', '&apos;', "'", '&quot;', '"', '&#38;', '&', '&#60;', '<', '&#62;', '>', '&#39;', "'", '&#34;', '"', '&#x26;', '&', '&#x3C;', '<', '&#x3E;', '>', '&#x27;', "'", '&#x22;', '"']
	return input.replace_each(escape_to_text)
}
