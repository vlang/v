struct Cfg {
	id   string
	name string
}

pub const cfg1 = Cfg{
	...cfg0
	name: 'name1'
}
pub const cfg0 = Cfg{
	id:   'cfg0'
	name: 'name'
}

fn test_main() {
	assert cfg1.id == 'cfg0'
}
