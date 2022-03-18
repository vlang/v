import toml
import toml.to

// Complete text from the example in the README.md:
// https://github.com/toml-lang/toml/blob/3b11f6921da7b6f5db37af039aa021fee450c091/README.md#Example
const toml_text = '# This is a TOML document.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]'

fn main() {
	doc := toml.parse_text(toml_text) or { panic(err) }
	title := doc.value('title').string()
	println('title: "$title"')
	ip := doc.value('servers.alpha.ip').string()
	println('Server IP: "$ip"')

	toml_json := to.json(doc)
	println(toml_json)
}
