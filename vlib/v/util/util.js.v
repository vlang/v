module util

pub fn skip_bom(file_content string) string {
	mut raw_text := file_content
	if raw_text.len >= 3 {
		js_text := raw_text.str
		_ := js_text
		#if (js_text.charCodeAt(0) == 0xEF && js_text.charCodeAt(1) == 0xBB && js_text.charCodeAt(2) == 0xBF)

		{
			#raw_text.str = js_text.slice(3,js_text.length);
		}
	}
	return raw_text
}
