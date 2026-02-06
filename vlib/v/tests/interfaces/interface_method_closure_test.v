interface Doer {
	job string
}

fn (d Doer) do() string {
	return '${d.job}'
}

struct Expector {
	do_fn fn () string @[required]
}

struct Actioner {
	job string
}

fn test_main() {
	a := &Actioner{'dier'}
	e := Expector{
		do_fn: Doer(a).do
	}
	assert e.do_fn() == 'dier'
}
