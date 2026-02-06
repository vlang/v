struct Structure1 {
	world [2]Structure2
}

struct Structure2 {
	liste []Structure3
}

struct Structure3 {
	coo []int
}

fn test_main() {
	app := Structure1{
		world: [2]Structure2{init: Structure2{
			liste: []Structure3{len: 1, init: Structure3{}}
		}}
	}
	assert app.world.len == 2
}
