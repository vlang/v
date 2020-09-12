module html

//import net.http

fn test_split_parse() {
	mut parser := Parser{}
	parser.initialize_all()
	parser.split_parse('<!doctype htm')
	parser.split_parse('l public')
	parser.split_parse('><html><he')
	parser.split_parse('ad><t')
	parser.split_parse('itle> Hum... ')
	parser.split_parse('A Tit')
	parser.split_parse('\nle</ti\ntle>')
	parser.split_parse('</\nhead><body>\t\t\t<h3>')
	parser.split_parse('Nice Test!</h3>')
	parser.split_parse('</bo\n\n\ndy></html>')
	parser.finalize()
	assert parser.get_tags().len == 11
	assert parser.get_tags()[3].get_content() == ' Hum... A Tit\nle'
}

fn test_giant_string() {
	mut temp_html := '<!doctype html><html><head><title>Giant String</title></head><body>'
	for counter := 0; counter < 2000; counter++ {
		temp_html += "<div id='name_$counter' class='several-$counter'>Look at $counter</div>"
	}
	temp_html += '</body></html>'
	mut parser := Parser{}
	parser.parse_html(temp_html, false)
	assert parser.get_tags().len == 4009
}

fn test_script_tag() {
	temp_html := "<html><body><script>\nvar googletag = googletag || {};\n
	googletag.cmd = googletag.cmd || [];if(3 > 5) {console.log('Birl');}\n</script></body></html>"
	mut parser := Parser{}
	parser.parse_html(temp_html, false)
	assert parser.get_tags()[2].get_content().len == 101
}

/*fn test_download_source() {
	println('Fetching github data in pastebin')
	resp := http.get('https://pastebin.com/raw/5snUQgqN') or {
		println('failed to fetch data from the server')
		return
	}
	println('Finalized fetching, start parsing')
	mut parser := Parser{}
	parser.parse_html(resp.text, false)
	assert parser.get_tags().len == 2244
}*/
