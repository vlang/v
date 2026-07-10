// KDL: coercion helpers — extract typed values from document model
import x.kdl

fn main() {
	src := '
sensor temp=22.5 unit="celsius" online=#true
server host="db.internal" port=5432 max-conn=1000
flags env=production debug=#false cache=#null
'
	doc := kdl.parse(src)!

	// Sensor node — float, string, bool
	sensor := doc.nodes[0]
	println('=== sensor ===')
	show_property(sensor, 'temp')
	show_property(sensor, 'unit')
	show_property(sensor, 'online')

	// Server node — int, string, large int
	server := doc.nodes[1]
	println('\n=== server ===')
	show_property(server, 'host')
	show_property(server, 'port')
	show_property(server, 'max-conn')

	// Flags — bool, null
	flags := doc.nodes[2]
	println('\n=== flags ===')
	show_property(flags, 'env')
	show_property(flags, 'debug')
	show_property(flags, 'cache')

	// Numeric tuple
	println('\n=== numeric ===')
	if val := kdl.property_get(&sensor, 'temp') {
		iv, fv, is_int := kdl.as_numeric(val)
		println('temp: i64=${iv}, f64=${fv}, is_int=${is_int}')
	}
}

fn show_property(node kdl.Node, key string) {
	if val := kdl.property_get(&node, key) {
		println('  ${key}:')
		println('    string: ${kdl.as_string(val)}')
		println('    int:    ${kdl.as_int(val)}')
		println('    f64:    ${kdl.as_f64(val)}')
		println('    bool:   ${kdl.as_bool(val)}')
		println('    null:   ${kdl.is_null(val)}')
	} else {
		println('  ${key}: <missing>')
	}
}
