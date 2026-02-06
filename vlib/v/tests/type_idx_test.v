// Note: .type_name() and .type_idx() called on an interface instance are more expensive
// than typeof(instance).name and typeof(instance).idx, since they will search and return
// the name and type index of the concrete interface instance.
//
// typeof(interface_instance).name returns the interface name, in the example here it will
// be always 'Animal'.
//
// interface_instance.type_name() will return 'Dog' or 'Cat' in this example, depending on
// what instance it is called.
//
// Similarly, typeof(interface_instance).idx will always return the same type index for all
// kinds of Animal.

interface Animal {
	name string
}

struct Dog {
	name string
}

struct Cat {
	name string
}

type SumType = int | string

fn test_type_idx() {
	d := Dog{
		name: 'Carlos'
	}
	c := Cat{
		name: 'Tom'
	}
	ad := Animal(d)
	ac := Animal(c)
	dump(ad)
	dump(ac)
	divider___()
	dump(typeof(ad).name)
	dump(typeof(ac).name)
	assert typeof(ad).name == 'Animal'
	assert typeof(ac).name == 'Animal'
	dump(typeof(ad).idx)
	dump(typeof(ac).idx)
	assert typeof(ad).idx == typeof(ac).idx
	divider___()
	dump(ad.type_name())
	dump(ac.type_name())
	assert ad.type_name() == 'Dog'
	assert ac.type_name() == 'Cat'
	dump(ad.type_idx())
	dump(ac.type_idx())
	assert ad.type_idx() != ac.type_idx()
	assert ac.type_idx() != typeof(ad).idx
	divider___()
	dump(typeof(d).name)
	dump(typeof(c).name)
	assert typeof(d).name == 'Dog'
	assert typeof(c).name == 'Cat'
	dump(typeof(d).idx)
	dump(typeof(c).idx)
	assert typeof(d).idx != typeof(c).idx
	assert typeof(d).idx != typeof(ad).idx
}

fn test_sumtype_type_idx() {
	s := 'abc'
	i := 123
	ss := SumType(s)
	si := SumType(i)
	divider___()
	dump(s)
	dump(i)
	dump(ss)
	dump(si)
	dump(typeof(s).idx)
	dump(typeof(i).idx)
	dump(typeof(ss).idx)
	dump(typeof(si).idx)
	assert typeof(ss).idx != typeof(s).idx
	assert typeof(s).idx != typeof(i).idx
	assert typeof(ss).idx == typeof(si).idx
	divider___()
	dump(ss.type_name())
	dump(si.type_name())
	assert ss.type_name() == 'string'
	assert si.type_name() == 'int'
	assert ss.type_name() != si.type_name()
	dump(ss.type_idx())
	dump(si.type_idx())
	assert ss.type_idx() == typeof(s).idx
	assert si.type_idx() == typeof(i).idx
	assert ss.type_idx() != si.type_idx()
	assert typeof(ss).idx != ss.type_idx()
	assert typeof(si).idx != si.type_idx()
}

fn divider___() {
	println('------------------------------------------------------------')
}
