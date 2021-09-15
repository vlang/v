import x.toml

const toml_text = '# Test TOML file

title = "TOML in V example"

[v]
name = "V"
open_sourced = 2019-06-22T20:20:28

[network]
ip = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true'

fn main() {
	doc := toml.parse(toml_text)
	title := doc.value('title').string()
	println('title: "$title"')
	net_ip := doc.value('network.ip').string()
	println('network IP: "$net_ip"')
}
