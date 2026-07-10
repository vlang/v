// KDL: relaxed mode — parse nginx-style config and other non-standard KDL
import kdl
import kdl.document

fn main() {
	// NGINX-style: / in values, \ in paths, () in names
	nginx_src := '
server {
  listen 80
  server_name example.com
  root /var/www/html
  location / {
    try_files $uri $uri/ /index.html
  }
  location /api {
    proxy_pass "http://backend:3000"
  }
}
'
	mut relaxed := kdl.RelaxedNonCompliant{
		flags: kdl.nginx_syntax
	}
	opts := kdl.ParseOpts{
		relaxed: relaxed
	}
	doc := kdl.parse_opts(nginx_src, opts)!

	println('nginx-style KDL:')
	walk(doc.nodes, 0)

	// Comment capture
	mut opts2 := kdl.ParseOpts{
		parse_comments: true
	}
	doc2 := kdl.parse_opts('// server configuration\nserver port=8080', opts2)!
	println('\nparse with comments:')
	for node in doc2.nodes {
		if comment := node.comment {
			if comment.before.len > 0 {
				println('  // ${comment.before}')
			}
			println('  ${node.name}')
		}
	}
}

fn walk(nodes []kdl.Node, depth int) {
	indent := '  '.repeat(depth)
	for node in nodes {
		println('${indent}${node.name}')
		for entry in node.entries {
			match entry {
				document.Argument {
					println('${indent}  arg: ${kdl.as_string(entry.value)}')
				}
				document.Property {
					println('${indent}  ${entry.key}=${kdl.as_string(entry.value)}')
				}
			}
		}
		if node.children.len > 0 {
			walk(node.children, depth + 1)
		}
	}
}
