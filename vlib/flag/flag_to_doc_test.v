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
		name:        'my app'
		version:     '1.0'
		description: 'My application'
	)! == doc3
}

const doc4 = 'flag_to_doc_test 1.0
--------------------------------------------------------------------------------
Flag to doc test.
Content here

Options:
  -v, --show-version        Show version and exit

  -d, --debug <int>         Debug level

  -l <f32>                  Level of lorem ipsum
                            and more
                            many many many more.
                            Notice how user newlines/format is kept since
                            input lines are all less or within
                            the default layout.description_padding
                            and max width

  --example <string>        Looong example text without newlines or anything
                            else and lorem ipsum and more and many many many
                            more. Should be auto fitted

  --square

  -m, -mmm... (can repeat)  This flag can be repeated

  -w, --wroom <int> (allowed multiple times)

  --the-limit <string>      Looongbobbytextwithoutnewlinesoranythingelseandlorem
                            ipsumandmoreandmanymanymanymore
                            ffffffffffffffffffffffffffffffff f

  -e, --extra               Secret flag that does not exist on the struct,
                            but we want documented (in same format as the
                            others)

  -q, --quiet-and-quite-long-flag <string>
                            Mega long description and secret flag that does
                            not exist on the struct, but we want documented.
                            Also the flag has custom newlines and the flag
                            line itself is super long

Footer content'

const doc5 = 'flag_to_doc_test 1.0
--------------------------------------------------------------------------------
Flag to doc test.
Content here

Options:
  -v, --show-version        Show version and exit
  -d, --debug <int>         Debug level
  -l <f32>                  Level of lorem ipsum
                            and more
                            many many many more.
                            Notice how user newlines/format is kept since
                            input lines are all less or within
                            the default layout.description_padding
                            and max width
  --example <string>        Looong example text without newlines or anything
                            else and lorem ipsum and more and many many many
                            more. Should be auto fitted
  --square
  -m, -mmm... (can repeat)  This flag can be repeated
  -w, --wroom <int> (allowed multiple times)
  --the-limit <string>      Looongbobbytextwithoutnewlinesoranythingelseandlorem
                            ipsumandmoreandmanymanymanymore
                            ffffffffffffffffffffffffffffffff f
  -e, --extra               Secret flag that does not exist on the struct,
                            but we want documented (in same format as the
                            others)
  -q, --quiet-and-quite-long-flag <string>
                            Mega long description and secret flag that does
                            not exist on the struct, but we want documented.
                            Also the flag has custom newlines and the flag
                            line itself is super long

Footer content'

@[name: 'flag_to_doc_test']
@[version: '1.0']
struct DocTest {
	show_version bool @[short: v; xdoc: 'Show version and exit']
	debug_level  int  @[long: debug; short: d; xdoc: 'Debug level']
	level        f32  @[only: l; xdoc: 'Override this doc string']
	example      string
	square       bool
	multi        int   @[only: m; repeats]
	wroom        []int @[short: w]
	the_limit    string
}

const field_docs = {
	'level':                                    'Level of lorem ipsum\nand more\nmany many many more.\nNotice how user newlines/format is kept since\ninput lines are all less or within\nthe default layout.description_padding\nand max width'
	'example':                                  'Looong example text without newlines or anything else and lorem ipsum and more and many many many more. Should be auto fitted'
	'the_limit':                                'Looongbobbytextwithoutnewlinesoranythingelseandlorem ipsumandmoreandmanymanymanymore ffffffffffffffffffffffffffffffff f'
	'multi':                                    'This flag can be repeated'
	'-e, --extra':                              'Secret flag that does not exist on the struct, but we want documented (in same format as the others)'
	'-q, --quiet-and-quite-long-flag <string>': 'Mega long description and secret flag that does not exist on the struct, but we want documented. Also the flag has custom newlines\nand the flag line itself is super long'
}

fn test_flag_to_doc_spacing_and_new_lines() {
	assert flag.to_doc[DocTest](
		description: 'Flag to doc test.
Content here'
		footer:      '
Footer content'
		fields:      unsafe { field_docs }
	)! == doc4

	// Test in compact mode also
	assert flag.to_doc[DocTest](
		options:     flag.DocOptions{
			compact: true
		}
		description: 'Flag to doc test.
Content here'
		footer:      '
Footer content'
		fields:      unsafe { field_docs }
	)! == doc5
}
