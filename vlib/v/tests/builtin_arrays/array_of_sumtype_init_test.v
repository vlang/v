pub type MenuItem = Action | Group | Separator

pub struct Group {
	children []MenuItem
}

pub struct Separator {}

pub struct Action {}

fn test_array_of_sumtype_init() {
	g := Group{
		children: [
			Action{},
			Separator{},
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
