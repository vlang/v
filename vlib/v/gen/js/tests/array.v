fn map_cb(s string) string { return 'CB: $s' }
fn filter_cb(n int) bool { return n < 4 }

fn variadic(args ...int) {
	println(args)
	println(args[0])
	println(args[1])
}

fn vararg_test() {
	variadic(1, 2, 3)
}

// TODO Remove `fn main` once vet supports scripts
fn main() {
vararg_test()

arr1 := ['Hello', 'JS', 'Backend']
mut arr2 := [1, 2, 3, 4, 5]

// Array slices
slice1 := arr1[1..3]
slice2 := arr2[..3]
slice3 := arr2[3..]

// Array indexes
idx1 := slice1[1]
arr2[0] = 1
arr2[0 + 1] = 2
println(arr2)

// TODO: This does not work for now
// arr2[0..1] = arr2[3..4]
// println(arr2)

// Array push operator
arr2 << 6
arr2 << [7, 8, 9]
println(arr2)
println('\n\n')

// String slices
mut slice4 := idx1[..4]
print('Back\t=> ')
println(slice4) // 'Back'

// String indexes
idx2 := slice4[0]
print('66\t=> ')
println(idx2)
// TODO:
// slice4[3] = `c`

// Maps
mut m := map[string]string
key := 'key'
m[key] = 'value'
val := m['key']
print('value\t=> ')
println(val)

// 'in' / '!in'
print('true\t=> ')
println('JS' in arr1)
print('false\t=> ')
println(3 !in arr2)
print('true\t=> ')
println('key' in m)
print('true\t=> ')
println('badkey' !in m)
print('true\t=> ')
println('o' in 'hello')

// for in
for _ in arr1 {}
println('0 to 8\t=>')
for i, _ in arr2 { println(i) }
println('\n\n4 to 5\t=> ')
for _, v in slice3 { println(v) }

println(int(1.5))

println('\n\n')

// map
a := arr1.map('VAL: $it')
b := arr1.map(map_cb)
c := arr1.map(map_cb(it))
d := arr1.map(fn(a string) string { return 'ANON: $a' })
// I don't know when this would ever be used,
// but it's what the C backend does ¯\_(ツ)_/¯
e := arr1.map(456)

println(a)
println(b)
println(c)
println(d)
println(e)

println('\n\n')

// filter
aa := arr2.filter(it < 4)
bb := arr2.filter(filter_cb)
cc := arr2.filter(filter_cb(it))
dd := arr2.filter(fn(a int) bool { return a < 4 })

println(aa)
println(bb)
println(cc)
println(dd)

// fixed arrays: implemented as normal arrays
f1 := [1, 2, 3, 4, 5]!!
mut f2 := [8]f32
f2[0] = f32(1.23)
f3 := ['foo', 'bar']!!
f4 := [u64(0xffffffffffffffff), 0xdeadface]!!

println('
$f1
$f2
$f3
$f4')
}
