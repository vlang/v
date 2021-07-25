fn main() {
	mut arr := [8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0]
	arr.sort()
	println(arr)
	arr[9] = 100
	arr[10] = 120
	arr.sort()

	println(arr)

	println([1, 2, 3, 4].repeat(2))

	println(arr.index(120))

	println([1, 2, 3, 4, 5].join('=>'))

	res := arr[4..7]

	println(res)
}
