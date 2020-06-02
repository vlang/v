fn variadic(args ...int) {
	println(args)
	println(args[0])
	println(args[1])
}

fn vararg_test() {
	variadic(1, 2, 3)
}

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

// String slices
mut slice4 := idx1[..4]
println(slice4) // 'Back'

// String indexes
idx2 := slice4[0]
println(idx2)
// TODO:
// slice4[3] = `c`

// Maps
mut m := map[string]string
key := 'key'
m[key] = 'value'
val := m['key']
println(val)

// 'in' / '!in'
println('JS' in arr1)
println(3 !in arr2)
println('key' in m)
println('badkey' !in m)
