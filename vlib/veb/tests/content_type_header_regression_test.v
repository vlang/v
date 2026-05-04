import net.http
import veb

fn test_set_header_content_type_preserves_charset_parameter() {
	mut ctx := veb.Context{
		res: http.Response{}
	}
	ctx.set_header(.content_type, 'text/html; charset=utf-8')

	ctx.text('Hello, World!')

	assert ctx.res.header.get(.content_type)! == 'text/html; charset=utf-8'
}
