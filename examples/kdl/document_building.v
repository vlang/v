// KDL: build documents programmatically and serialize via format()
import x.kdl

fn main() {
	mut doc := kdl.Document{}

	doc.nodes << kdl.Node{
		name:     'server'
		entries:  [
			kdl.Property{
				key:   'host'
				value: kdl.StringVal{
					value: '0.0.0.0'
				}
			},
			kdl.Property{
				key:   'port'
				value: kdl.IntVal{
					value: 8080
				}
			},
		]
		children: [
			kdl.Node{
				name:    'tls'
				entries: [
					kdl.Property{
						key:   'enabled'
						value: kdl.BoolVal{
							value: true
						}
					},
					kdl.Property{
						key:   'cert'
						value: kdl.StringVal{
							value: '/etc/ssl/cert.pem'
						}
					},
				]
			},
			kdl.Node{
				name:     'routes'
				children: [
					kdl.Node{
						name:    'route'
						entries: [
							kdl.Argument{
								value: kdl.StringVal{
									value: '/'
								}
							},
							kdl.Property{
								key:   'handler'
								value: kdl.StringVal{
									value: 'index'
								}
							},
						]
					},
					kdl.Node{
						name:    'route'
						entries: [
							kdl.Argument{
								value: kdl.StringVal{
									value: '/api'
								}
							},
							kdl.Property{
								key:   'handler'
								value: kdl.StringVal{
									value: 'api'
								}
							},
						]
					},
				]
			},
		]
	}

	doc.nodes << kdl.Node{
		name:    'logging'
		entries: [
			kdl.Property{
				key:   'level'
				value: kdl.StringVal{
					value: 'info'
				}
			},
			kdl.Property{
				key:   'format'
				value: kdl.StringVal{
					value: 'json'
				}
			},
		]
	}

	out := kdl.format(doc)!
	print(out)

	// Verify round-trip
	doc2 := kdl.parse(out)!
	println('\n--- round-trip OK: ${doc2.nodes.len} top-level nodes ---')
}
