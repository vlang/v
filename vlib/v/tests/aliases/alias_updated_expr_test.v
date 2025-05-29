module main

struct Cfg {
	item string
}

type AliasCfg = Cfg

fn foo(cfg &AliasCfg) {
	var := &AliasCfg{
		...cfg
		item: 'foo'
	}
	assert var.item == 'foo'
}

fn test_main() {
	foo(AliasCfg{
		item: 'bar'
	})
}
