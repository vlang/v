net/http is an HTML written in pure V.

## Usage
```v oksyntax
import net.html

fn main() {
	doc := html.parse('<html><body><h1 class="title">Hello world!</h1></body></html>')
	tag := doc.get_tag('h1')[0] // <h1>Hello world!</h1>
	println(tag.name) // h1
	println(tag.content) // Hello world!
	println(tag.attributes) // {'class':'title'}
	println(tag.str()) // <h1 class="title">Hello world!</h1>
}
```
More examples found on [`parser_test.v`](parser_test.v) and [`html_test.v`](html_test.v)
