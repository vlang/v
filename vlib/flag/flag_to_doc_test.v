import flag

const doc1 = 'an app 1.2.3
--------------------------------------------------------
Some config struct

Options:
  -v, --show-version        Show version and exit

  -d, --debug <int>         Debug level

  -l <f32>                  This doc text is overwritten
Use ESC to exit'

const doc2 = 'an app 1.2.3
-----------------------------------------------------------------------------
Some config struct

Options:
  -v, --show-version        Show version and exit

  -d, --debug <int>         Debug level

  -l <f32>                  This is a doc string of the field `level` on
                            struct `Config`

  -e, --extra               Extra flag that does not exist on the struct, but
                            we want documented (in same format as the others)
Use ESC to exit'

const doc3 = 'my app 1.0
--------------------------------------------------------
My application

Options:
  -v, --show-version        Show version and exit

  -d, --debug <int>         Debug level

  -l <f32>                  This doc text is overwritten
Use ESC to exit'

@[xdoc: 'Some config struct']
@[footer: 'Use ESC to exit']
@[name: 'an app']
@[version: '1.2.3']
struct Config {
	show_version bool @[short: v; xdoc: 'Show version and exit']
	debug_level  int  @[long: debug; short: d; xdoc: 'Debug level']
	level        f32  @[only: l; xdoc: 'This doc text is overwritten']
}

fn test_direct_to_doc() {
	assert flag.to_doc[Config]()! == doc1
}

fn test_attrs() {
	assert flag.to_doc[Config](
		fields: {
			'level':       'This is a doc string of the field `level` on struct `Config`'
			'-e, --extra': 'Extra flag that does not exist on the struct, but we want documented (in same format as the others)'
		}
	)! == doc2
}

fn test_attrs_override() {
	assert flag.to_doc[Config](
		name: 'my app'
		version: '1.0'
		description: 'My application'
	)! == doc3
}
