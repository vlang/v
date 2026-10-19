net/html is an **HTML Parser** written in pure V.

## Usage

```v
import net.html

fn main() {
	doc := html.parse('<html><body><h1 class="title">Hello world!</h1></body></html>')
	tag := doc.get_tags(name: 'h1')[0] // <h1>Hello world!</h1>
	println(tag.name) // h1
	println(tag.content) // Hello world!
	println(tag.attributes) // {'class':'title'}
	println(tag.str()) // <h1 class="title">Hello world!</h1>
}
```

For tags with nested markup, `tag.content` preserves the inner HTML:

```v
import net.html

fn main() {
	doc := html.parse('<p>before <span>in between</span> after</p>')
	tag := doc.get_tags(name: 'p')[0]
	println(tag.content) // before <span>in between</span> after
	println(tag.text()) // before in between after
}
```

More examples found on [`parser_test.v`](parser_test.v) and [`html_test.v`](html_test.v)

Comments are omitted from the parsed tree. Text before and after comments remains in order,
including text wrapped by hydration markers such as `<!--[-->hello<!--]-->`. Comments do not
create empty text nodes or change the surrounding elements.

Calling `get_tag_by_class_name` without class names returns `none`;
`get_tags_by_class_name` without class names returns an empty array.

Script and style elements preserve their raw bodies in `tag.content` and when serialized with
`tag.str()`. Text following a closing script or style tag belongs to the surrounding document.

Document searches include the root element in name, class, and attribute matches. Class searches
match all supplied class names and return elements in document order. Searches on a `Tag` inspect
only its descendants.

Attribute searches match attributes by presence, including boolean attributes such as `disabled`
and attributes with an empty value. Searching for an empty value matches only tags that have the
attribute. Searches on a `Tag` recursively inspect its children.

Unterminated script and style elements retain their collected body when parsing ends.

Newlines inside tags separate names and attributes like spaces. Consecutive separators preserve
a pending quoted attribute value.

Trailing text is retained when parsing ends, including text after a closing tag and text in an
unclosed element. Text after a closing tag belongs to the surrounding element.

Attribute names are case insensitive. Quoted and unquoted attribute values preserve their case,
and both forms populate class selectors. HTML whitespace separates attributes; whitespace around
`=` is accepted. Empty attribute names are omitted.
