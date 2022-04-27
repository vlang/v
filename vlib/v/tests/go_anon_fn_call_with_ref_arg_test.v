import net.html
import net.http
import vweb

struct App {
	vweb.Context
}

struct HtmlData {
mut:
	inner_html string
	data       []&HtmlData
}

fn convert_to_html_data(tag html.Tag) &HtmlData {
	println('convert_to_html_data')
	if tag.children.len == 0 {
		return &HtmlData{
			inner_html: tag.content
			data: []
		}
	}

	mut node := &HtmlData{
		inner_html: tag.content
		data: []&HtmlData{}
	}

	for child in tag.children {
		child_data := convert_to_html_data(child)
		node.data << child_data
	}
	return node
}

fn test_go_anon_fn_call_with_ref_arg() {
	app := App{}
	// vweb.run(app, 8081)
	print('Hello, world!')
	assert true
}

['/index']
pub fn (mut app App) index() vweb.Result {
	res := http.get('https://www.nzx.com/markets/NZSX') or {
		http.new_response(status: .internal_server_error)
	}
	mut data := html.parse(res.text)
	table := data.get_tag_by_attribute_value('class', 'small-12 medium-8 columns content')[0]

	go fn (table html.Tag) {
		final_table := convert_to_html_data(table)
		println(final_table)
	}(table)

	return app.html(table.children.str())
}
