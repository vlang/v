module html

import strings

fn test_split_parse() {
	mut parser := Parser{}
	parser.init()
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
	assert parser.tags.len == 11
	assert parser.tags[3].content == ' Hum... A Tit\nle'
}

fn test_giant_string() {
	mut temp_html := strings.new_builder(200)
	mut parser := Parser{}
	temp_html.write('<!doctype html><html><head><title>Giant String</title></head><body>')
	for counter := 0; counter < 2000; counter++ {
		temp_html.write("<div id='name_$counter' class='several-$counter'>Look at $counter</div>")
	}
	temp_html.write('</body></html>')
	parser.parse_html(temp_html.str())
	assert parser.tags.len == 4009
}

fn test_script_tag() {
	mut parser := Parser{}
	script_content := "\nvar googletag = googletag || {};\ngoogletag.cmd = googletag.cmd || [];if(3 > 5) {console.log(\'Birl\');}\n"
	temp_html := '<html><body><script>$script_content</script></body></html>'
	parser.parse_html(temp_html)
	assert parser.tags[2].content.len == script_content.replace('\n', '').len
}
