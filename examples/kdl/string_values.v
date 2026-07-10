// KDL: string values — quoted, bare, raw, multiline, escapes
import kdl

fn main() {
	// Quoted strings with escapes
	doc := kdl.parse('strings "hello\\nworld" simple bare r #"raw\\n"#')!
	node := doc.nodes[0]
	for i in 0 .. node.entries.len {
		e := node.entries[i]
		println('entry ${i}: type=${e.type_name()}, value="${kdl.as_string(e.value)}"')
	}

	// Multiline string with dedent
	src := 'multiline """
    line one
    line two
    """'
	doc2 := kdl.parse(src)!
	v := doc2.nodes[0].entries[0].value
	println('\nmultiline value: "${kdl.as_string(v)}"')

	// Multiline raw string (no escape processing)
	src2 := 'raw-multi #"""
    literal \\n\\t\\r here
    """#'
	doc3 := kdl.parse(src2)!
	v2 := doc3.nodes[0].entries[0].value
	println('multiline raw: "${kdl.as_string(v2)}"')

	// Empty string
	doc4 := kdl.parse('empty ""')!
	println('empty string: "${kdl.as_string(doc4.nodes[0].entries[0].value)}"')
}
