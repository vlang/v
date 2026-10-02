// A tour of the x.json5 module: parsing, decoding, document access and
// encoding.
import x.json5

struct Config {
	name  string
	port  int = 8080
	ratio f64
	tags  []string
}

struct Money {
	cents int
}

fn (m Money) to_json5() string {
	return '\$${m.cents / 100}.${m.cents % 100:02}'
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
	value := json5.parse(text) or { panic(err) }
	root := value.as_map()
	println('name = ${root['name'] or { json5.null }.string()}')
	println('port = ${root['port'] or { json5.null }.int()}')
	println('tags = ${root['tags'] or { json5.null }.array().as_strings()}')
}

// decode converts a document into a V type. Missing keys keep the field
// default, so a partial document is valid input.
fn typed() {
	cfg := json5.decode[Config]('{ name: "srv", ratio: 1.5 }') or { panic(err) }
	println('name  = ${cfg.name}')
	println('port  = ${cfg.port}') // 8080, the default
	println('ratio = ${cfg.ratio}')
}

// doc keeps the tree and supports path lookup and lenient reflection.
fn document() {
	text := '{ servers: [{ host: "a" }, { host: "b" }] }'
	doc := json5.parse_text(text) or { panic(err) }
	second := doc.get('servers[1].host') or { panic('missing servers[1].host') }
	println('second host = ${second.string()}')

	// reflect leaves an unmatched field at its default, decode reports the error.
	partial := json5.parse_text('{ name: "srv", port: "not a number" }') or { panic(err) }
	println('reflected port = ${partial.reflect[Config]().port}')
}

// encode writes compact output, which is also valid JSON. A type with a
// `to_json5` method controls its own text.
fn encoding() {
	println(json5.encode(Config{
		name:  'srv'
		port:  8080
		ratio: 1.5
		tags:  ['a', 'b']
	}))
	println(json5.encode(Money{ cents: 1999 }))

	opts := json5.EncodeOpts{
		indent:          '  '
		single_quotes:   true
		unquoted_keys:   true
		trailing_commas: true
	}
	tree := json5.parse('{ a: [1, 2], }') or { panic(err) }
	println(json5.encode_with_opts(tree, opts))
}

fn main() {
	dynamic()
	typed()
	document()
	encoding()
}
