// KDL: line continuation — spread nodes across multiple lines with \
import x.kdl
import x.kdl.document

fn main() {
	// Line continuation lets you break long nodes across multiple lines.
	// A backslash at end-of-line consumes the newline and joins the content.

	src := 'server \\\n  host="localhost" \\\n  port=8080 \\\n  max-conn=1000\n\nnode \\\n  \\\n  arg1 arg2'

	doc := kdl.parse(src)!

	println('Parsed ${doc.nodes.len} top-level nodes:')
	for node in doc.nodes {
		println('')
		println('  [${node.name}] (${node.entries.len} entries, ${node.children.len} children)')
		for entry in node.entries {
			match entry {
				document.Argument {
					println('    arg: ${kdl.as_string(entry.value)}')
				}
				document.Property {
					println('    prop: ${entry.key}=${kdl.as_string(entry.value)}')
				}
			}
		}
	}
}
