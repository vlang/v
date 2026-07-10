// KDL: read from and write to files — parse_file, parse_file_opts, format → os.write_file
import x.kdl
import x.kdl.document
import os

fn main() {
	mut doc := kdl.Document{}
	doc.nodes << kdl.Node{
		name:    'app'
		entries: [
			document.Property{
				key:   'name'
				value: document.StringVal{
					value: 'my-server'
				}
			},
			document.Property{
				key:   'port'
				value: document.IntVal{
					value: 8080
				}
			},
			document.Property{
				key:   'debug'
				value: document.BoolVal{
					value: true
				}
			},
		]
	}
	doc.nodes << kdl.Node{
		name:    'database'
		entries: [
			document.Property{
				key:   'host'
				value: document.StringVal{
					value: 'db.internal'
				}
			},
			document.Property{
				key:   'pool-size'
				value: document.IntVal{
					value: 20
				}
			},
		]
	}

	kdl_text := kdl.format(doc)!
	file_path := os.join_path(os.temp_dir(), 'kdl_demo_config.kdl')
	os.write_file(file_path, kdl_text)!
	println('Written to: ${file_path}')
	println('--- file content ---')
	println(kdl_text)

	loaded := kdl.parse_file(file_path)!
	println('--- read back ---')
	println('nodes: ${loaded.nodes.len}')

	for node in loaded.nodes {
		println('  [${node.name}]')
		for entry in node.entries {
			match entry {
				document.Property {
					println('    ${entry.key} = ${kdl.as_string(entry.value)}')
				}
				document.Argument {}
			}
		}
	}

	rt_text := kdl.format(loaded)!
	roundtripped := kdl.parse(rt_text)!
	assert roundtripped.nodes.len == loaded.nodes.len
	println('--- round-trip OK: ${roundtripped.nodes.len} nodes ---')

	commented_src := '// server configuration\nserver host="0.0.0.0" port=443'
	commented_path := os.join_path(os.temp_dir(), 'kdl_demo_commented.kdl')
	os.write_file(commented_path, commented_src)!

	opts := kdl.ParseOpts{
		parse_comments: true
	}
	cdoc := kdl.parse_file_opts(commented_path, opts)!
	println('--- parse_file_opts with comments ---')
	for node in cdoc.nodes {
		if comment := node.comment {
			if comment.before.len > 0 {
				println('  // ${comment.before}')
			}
		}
		println('  ${node.name}')
		for entry in node.entries {
			match entry {
				document.Property {
					println('    ${entry.key} = ${kdl.as_string(entry.value)}')
				}
				document.Argument {}
			}
		}
	}

	// Re-parse to get a mutable copy
	mut modified := kdl.parse(rt_text)!
	modified.nodes[0].entries << document.Property{
		key:   'log-level'
		value: document.StringVal{
			value: 'info'
		}
	}
	updated_path := os.join_path(os.temp_dir(), 'kdl_demo_updated.kdl')
	os.write_file(updated_path, kdl.format(modified)!)!
	println('--- updated config written to: ${updated_path} ---')

	// Verify the update by reading it back
	reloaded := kdl.parse_file(updated_path)!
	println('--- verify update ---')
	app_node := reloaded.nodes[0]
	println('app properties: ${app_node.entries.len}')
	for entry in app_node.entries {
		match entry {
			document.Property {
				println('  ${entry.key} = ${kdl.as_string(entry.value)}')
			}
			document.Argument {}
		}
	}

	// Clean up temp files
	os.rm(file_path) or {}
	os.rm(commented_path) or {}
	os.rm(updated_path) or {}
	println('\nall file I/O demos completed successfully.')
}
