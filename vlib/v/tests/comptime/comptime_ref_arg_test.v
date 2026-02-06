// Declare structures covering a subset of a HAR file

struct HarContent {
	size      i64
	mime_type string @[json: 'mimeType']
}

struct HarResponse {
	content HarContent
}

struct HarEntry {
	response HarResponse
}

pub struct HarLog {
	entries []HarEntry
}

struct Har {
	log HarLog
}

// Declare function printing object contents using generics

fn show[T](val T) {
	$if T is string {
		show_string(val)
	} $else $if T is $array {
		show_array(val)
	} $else $if T is $struct {
		show_struct(&val)
	} $else {
		print('primitive: ${val.str()}')
	}
}

fn show_array[T](array []T) {
	println('array []${T.name}')
	for i, item in array {
		println('item ${i}')
		show(item)
		println('')
	}
}

fn show_struct[T](object &T) {
	println('struct ${T.name}')
	$for field in T.fields {
		mut json_name := field.name
		for attr in field.attrs {
			if attr.starts_with('json: ') {
				json_name = attr[6..]
			}
		}

		print('key: ')
		show_string(json_name)
		println('')

		println('value ${T.name}.${field.name} (json: ${json_name}), field.typ: ${field.typ}')
		$if field.typ is string {
			print('string: ')
			show_string(object.$(field.name))
		} $else $if field.is_array {
			show_array(object.$(field.name))
		} $else $if field.is_struct {
			item := object.$(field.name)
			show_struct(&item)
		} $else {
			print('primitive: ')
			print(object.$(field.name).str())
		}
		println('')
	}
}

fn show_string(s string) {
	print(`"`)
	print(s)
	print(`"`)
}

fn test_main() {
	har := Har{
		log: HarLog{
			entries: [
				HarEntry{
					response: HarResponse{
						content: HarContent{
							size:      48752
							mime_type: 'text/html'
						}
					}
				},
			]
		}
	}
	show(har)
	assert true
}
