import json

struct Abc {
	my_ints [6]int
	my_strs [2]string
	my_arr  []int
}

struct Fixed_Array {
	abc [5]Abc
}

fn test_json_serialisation_of_fixed_arrays() {
	a := Fixed_Array{[
		Abc{
			my_ints: [1, 2, 3, 4, 5, 6]!
			my_strs: ['zzzddddddddddd', 'www']!
			my_arr: [1, 2, 3]
		},
		Abc{
			my_ints: [7, 8, 9, 10, 11, 12]!
			my_strs: ['zzz', 'www']!
			my_arr: [1, 2, 3]
		},
		Abc{
			my_ints: [13, 14, 15, 16, 17, 18]!
			my_strs: ['zzzaaaaaaaa', 'www111111112333']!
			my_arr: [1, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 7, 1, 2, 3, 4, 5]
		},
		Abc{
			my_ints: [19, 21, 23, 10, 50444, 3331]!
			my_strs: ['zzz', 'www']!
			my_arr: [1, 2, 3]
		},
		Abc{
			my_ints: [20, 22, 24, 44, 560, 640]!
			my_strs: ['zzz', 'www']!
			my_arr: [1, 2, 3]
		},
	]!}
	s := json.encode(a)
	dump(s)
	b := json.decode(Fixed_Array, s)!
	dump(b)
	assert a == b
}
