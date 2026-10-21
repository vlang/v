import toml.ast
import toml.parser
import toml.scanner

fn test_datetime_ast_preserves_public_date_and_time_components() {
	for text in [
		'1979-05-27T07:32:00Z',
		'1979-05-27t07:32:00z',
		'1979-05-27 07:32:00Z',
		'1979-05-27T00:32:00.999999-07:00',
		'1979-05-27T00:32:00+07:00',
		'1979-05-27T07:32:00',
	] {
		mut source := scanner.new_simple_text('value = ${text}')!
		mut p := parser.new_parser(parser.Config{ scanner: &source })
		root := p.parse()!
		table := root.table as map[string]ast.Value
		parsed := table['value'] or { panic('missing datetime') }
		assert parsed is ast.DateTime
		dt := parsed as ast.DateTime
		assert dt.text == text
		assert dt.date.text == text[..10]
		assert dt.time.text == text[11..]
		assert dt.date.pos.pos == dt.pos.pos
		assert dt.time.pos.pos >= dt.date.pos.pos + 10
		assert dt.str().contains("  date:  '${dt.date}'\n")
		assert dt.str().contains("  time:  '${dt.time}'\n")
	}
}

fn test_datetime_ast_accepts_existing_component_initializers() {
	dt := ast.DateTime{
		text: '2000-01-02T03:04:05'
		date: ast.Date{ text: '2000-01-02' }
		time: ast.Time{ text: '03:04:05' }
	}
	assert dt.date.text == '2000-01-02'
	assert dt.time.text == '03:04:05'
}
