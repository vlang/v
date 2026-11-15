// `make` runs this program with the same compiler and GC flags as the test
// warmup. Bootstrap helpers may use -gc none, which can select different cache
// entries from ordinary native programs using the default GC.
import strconv
import strings

fn main() {
	value := strconv.atoi('42') or { panic(err) }
	assert value == 42
	assert strings.repeat(120, 2) == 'xx'
}
