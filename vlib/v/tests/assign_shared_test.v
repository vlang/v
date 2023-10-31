struct AA {
	a shared []int
}

fn test_assign_shared() {
	a := AA{[1]}
	lock a.a {
		a.a = []int{cap: 10}
	}
	lock a.a {
		assert a.a == []
	}
}
