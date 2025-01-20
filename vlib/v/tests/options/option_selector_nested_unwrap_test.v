module main

type Foo = string | int | f32

struct Svc {
mut:
	log ?Foo
}

fn t(v string) !string {
	return v
}

fn Svc.init(log ?Foo) Svc {
	mut svc := Svc{
		log: log
	}
	if svc.log != none {
		if svc.log is string {
			assert svc.log.str() == 'foo'
			_ := t(svc.log) or { panic(err) }
		} else {
			assert false
		}
		assert true
	}
	return svc
}

struct CSvc {
	Svc
pub mut:
	log ?Foo
}

pub fn CSvc.init(log ?Foo) CSvc {
	mut c := CSvc{
		log: log
	}
	c.Svc = Svc.init(log)
	return c
}

fn test_main() {
	CSvc.init('foo')
}
