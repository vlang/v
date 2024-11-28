@[type]
enum Nums {
	one = 1
	two = 2
	ten = 10
}

fn test_main() {
	mut str := []string{}
	$for n in Nums.values {
		str << '${n.name} ${n.value} ${n.value:d} ${int(n.value)} | ${n.value:o} ${n.value:x} ${n.value:X} ${n.value:b}'
	}
	assert str == ['one one 1 1 | 1 1 1 1', 'two two 2 2 | 2 2 2 10', 'ten ten 10 10 | 12 a A 1010']
}
