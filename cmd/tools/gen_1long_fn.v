import strings
import os

n := os.args[1] or { '1_000_000' }.int()
stmt := os.args[2] or { 'a = b + c * d' }

mut s := strings.new_builder(1000)
s.writeln('fn main() {')
s.writeln('    mut a,b,c,d := 0,1,2,3')
for _ in 0 .. n {
	s.writeln('    ${stmt}')
}
s.writeln('    println("a: \${a}")')
s.writeln('}')
os.write_file('${n}.v', s.str())!
