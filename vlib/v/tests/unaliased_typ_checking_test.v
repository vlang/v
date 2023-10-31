type MyArray = []string
type MyString = string
type MyFloat = f64
type MyMap = map[string]string

struct Foo {
	a string
	b ?string
	c MyArray
	d MyString
	e MyFloat
	f MyMap
}

fn test_main() {
	mut out := map[string][]string{}
	$for field in Foo.fields {
		print('${field.name} is ')
		$if field.unaliased_typ is $int {
			println('numeric')
			out[field.name] << 'numeric'
		} $else $if field.unaliased_typ is $array {
			println('array')
			out[field.name] << 'array'
		} $else $if field.unaliased_typ is $float {
			println('float')
			out[field.name] << 'float'
		} $else $if field.unaliased_typ is $map {
			println('map')
			out[field.name] << 'map'
		} $else $if field.unaliased_typ is string || field.unaliased_typ is ?string {
			println('string opt? ${field.is_option}')
			out[field.name] << 'string'
		} $else {
			assert false
		}
	}
	assert out['a'][0] == 'string'
	assert out['b'][0] == 'string'
	assert out['c'][0] == 'array'
	assert out['d'][0] == 'string'
	assert out['e'][0] == 'float'
	assert out['f'][0] == 'map'
}
