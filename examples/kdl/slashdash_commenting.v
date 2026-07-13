// KDL: slashdash comments — comment out nodes, arguments, properties, and children blocks
import x.kdl
import x.kdl.document

fn main() {
	src := '
// The slashdash "/-" prefix lets you "comment out" parts of a KDL document
// without actually deleting them. They behave like whitespace when parsed.

/- disabled_feature       // ← entire node (including children) is ignored
    enabled=true
    { sub-config }

enabled_feature "on"     // ← this one is parsed normally

server /- old-port        // ← "old-port" argument is ignored
    8080                  // ← "8080" is the first real argument
    /- debug=#true        // ← the whole debug=true property is ignored
    host="localhost"      // ← this property is included

parent {
    /- { deprecated }     // ← child block and all its children are ignored
    { active }            // ← this child is included
}
'

	doc := kdl.parse(src)!

	println('Nodes parsed: ${doc.nodes.len}')
	for node in doc.nodes {
		println('')
		println('[${node.name}]  (entries: ${node.entries.len}, children: ${node.children.len})')
		for entry in node.entries {
			match entry {
				document.Argument {
					println('  arg: ${kdl.as_string(entry.value)}')
				}
				document.Property {
					println('  prop: ${entry.key}=${kdl.as_string(entry.value)}')
				}
			}
		}
		for child in node.children {
			println('  └ [${child.name}]')
		}
	}

	// Slashdash can also comment out type-annotated arguments
	doc2 := kdl.parse('data (u8)/- 1 2')!
	println('\n--- slashdash on type-annotated arg ---')
	println('args: ${doc2.nodes[0].entries.len}') // only "2" remains
}
