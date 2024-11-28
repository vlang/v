// This example shows how to use V's compile time defines and values via the -d flag and $d() function.
//
// To change the default values below, pass compile flags to v using: `-d ident=<value>`
// Examples:
// ´´´
// v -d header -d pad=10 -d jobs=8 run d_compile_value.v
// v -d id="Bob" -d pad=2 -d pad_char='$' -d jobs=10 run d_compile_value.v
// ```

const pad = $d('pad', 5)
const pad_char = $d('pad_char', `-`)

const footer = '
Available compile time flags:
 -d pad=<i64>
 -d pad_char=<character>
 -d id="<string>"
 -d jobs=<i64>
 -d header=<bool>
You can turn this message off with:
 -d footer=false'

struct Job {
	id string = $d('id', 'Job') // adjust with `-d id="My ID"`
}

struct App {
	jobs [$d('jobs', 4)]Job // adjust fixed array size with `-d jobs=6`
}

// println_padded prints `str` to stdout with a padding.
// Padding length and padding character can be adjusted at compile time
// via `-d pad=1` and `-d pad_char=x`.
fn println_padded(str string) {
	for _ in 0 .. pad {
		print(rune(pad_char))
	}
	println(' ${str}')
}

fn main() {
	header := $d('header', false) // adjust header variable with `-d header=true` or simply `-d header`
	if header {
		println_padded('Compile Value Example')
	}

	app := App{}
	for i, job in app.jobs {
		println_padded('${job.id} ${i + 1}')
	}

	// Turn the footer message off using `-d footer=false`
	if $d('footer', true) {
		println(footer)
	}
}
