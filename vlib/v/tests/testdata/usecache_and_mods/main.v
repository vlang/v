import v.tests.testdata.usecache_and_mods.xx
import v.tests.testdata.usecache_and_mods.aaa
import strconv
import strings

const used = aaa.used + xx.used

fn main() {
	println(used)
	println(strconv.c_ten)
	mut sb := strings.new_builder(1024)
	sb.writeln('hello')
	sb.writeln('world')
	print(sb.str())
	println('----- done ----')
}
