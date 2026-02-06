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

More examples found on [`parser_test.v`](parser_test.v) and [`html_test.v`](html_test.v)
