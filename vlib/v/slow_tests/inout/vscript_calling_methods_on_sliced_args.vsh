// `args` is the unqualified `os.args` in a script; methods called directly on
// a slice of it must see `[]string`, not `string`.
// A parameter named `args` still shadows `os.args`.
fn doubled_tail(args []int) []int {
	return args[1..].map(it * 2)
}

println(args#[1..].join(' ') == '')
println(args[1..].join(',').len)
println(args[..1].join('') == args[0])
println((args#[..1]).filter(it.len > 0).len)
println(args[..1].map(it == args[0]))
println(doubled_tail([1, 2, 3]))
