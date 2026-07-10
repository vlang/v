// KDL: basic parse, iterate nodes, walk children, format back to text
import kdl
import kdl.document

fn main() {
	// Parse a KDL document
	data := '
package {
  name "my-app"
  version "1.2.3"
  author "Alice"
  dependencies {
    libA "^3.2"
    libB "^1.0"
  }
  scripts {
    build "v ."
    test "v test ."
  }
}
'
	doc := kdl.parse(data)!

	println('Top-level node: ${doc.nodes[0].name}')
	node := doc.nodes[0]

	// Walk properties
	for entry in node.entries {
		match entry {
			document.Property {
				println('  ${entry.key} = ${kdl.as_string(entry.value)}')
			}
			document.Argument {}
		}
	}

	// Walk children recursively
	walk_children(node, 1)

	// Round-trip: serialize back to text
	println('\n--- formatted ---')
	print(kdl.format(doc)!)
}

fn walk_children(node kdl.Node, depth int) {
	for child in node.children {
		indent := '  '.repeat(depth)
		println('${indent}└ ${child.name}')
		for entry in child.entries {
			match entry {
				document.Argument {
					println('${indent}    arg: ${kdl.as_string(entry.value)}')
				}
				document.Property {
					println('${indent}    ${entry.key}=${kdl.as_string(entry.value)}')
				}
			}
		}
		walk_children(child, depth + 1)
	}
}
