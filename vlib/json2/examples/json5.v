// A tour of the json2 JSON5 support module: parsing, decoding, document access and
// encoding.
import json2

struct Config {
	name  string
	port  int = 8080
	ratio f64
	tags  []string
}

struct Money {
	cents int
}

fn (m Money) to_json() string {
	return json2.encode('\$${m.cents / 100}.${m.cents % 100:02}')
}

// parse returns the dynamic Any tree.
fn dynamic() {
	text := '{
	// JSON5 allows comments and unquoted keys.
	name: "srv",
	port: 0x1F90,          // 8080 in hexadecimal
	ratio: .5,
	tags: [ "a", \'b\', ],  // trailing commas are fine
}'
	value := json2.parse(text, json5: true) or { panic(err) }
	root := value.as_map()
	println('name = ${root['name'] or { json2.null }.str()}')
	println('port = ${root['port'] or { json2.null }.int()}')
	println('tags = ${root['tags'] or { json2.null }.as_array().map(it.str())}')
}

// decode converts a document into a V type. Missing keys keep the field
// default, so a partial document is valid input.
fn typed() {
	cfg := json2.decode[Config]('{ name: "srv", ratio: 1.5 }', json5: true) or { panic(err) }
	println('name  = ${cfg.name}')
	println('port  = ${cfg.port}') // 8080, the default
	println('ratio = ${cfg.ratio}')
}

// doc keeps the tree and supports path lookup and lenient reflection.
fn document() {
	text := '{ servers: [{ host: "a" }, { host: "b" }] }'
	doc := json2.parse_text(text, json5: true) or { panic(err) }
	second := doc.get('servers[1].host') or { panic('missing servers[1].host') }
	println('second host = ${second.str()}')

	// reflect leaves an unmatched field at its default, decode reports the error.
	partial := json2.parse_text('{ name: "srv", port: "not a number" }', json5: true) or { panic(err) }
	println('reflected port = ${partial.reflect[Config]().port}')
}

// encode writes compact output, which is also valid JSON. A type with a
// `to_json` method controls its own text.
fn encoding() {
	println(json2.encode(Config{
		name:  'srv'
		port:  8080
		ratio: 1.5
		tags:  ['a', 'b']
	}))
	println(json2.encode(Money{ cents: 1999 }))

	opts := json2.EncoderOptions{
		json5:           true
		prettify:        true
		indent_string:   '  '
		single_quotes:   true
		unquoted_keys:   true
		trailing_commas: true
	}
	tree := json2.parse('{ a: [1, 2], }', json5: true) or { panic(err) }
	println(json2.encode(tree, opts))
}

fn main() {
	dynamic()
	typed()
	document()
	encoding()
}
