pub type MenuItem = Action | Group | Separater

pub struct Group {
	children []MenuItem
}

pub struct Separater {}

pub struct Action {}

fn test_array_of_sumtype_init() {
	g := Group{
		children: [
			Action{},
			Separater{},
			Group{
				children: [
					Action{},
				]
			},
		]
	}
	println(g)
	assert g.children.len == 3
}
