struct Struc {}

type StrucOpt = ?Struc

// vfmt off
struct StrucContainer {
	a [](?Struc)
	b []?Struc
	c ?[]Struc
	d [](?Struc) = [](?Struc){}
	e []?Struc = []?Struc{}
}
// vfmt on

fn test_par_decl_types() {
	a := []?Struc{}
	b := []?Struc{}
	c := []StrucOpt{}

	if typeof(a).idx != typeof[[]?Struc]().idx {
		assert false
	}
	if typeof(a).idx != typeof[[]?Struc]().idx {
		assert false
	}
	if typeof(b).idx != typeof[[]?Struc]().idx {
		assert false
	}
	if typeof(c).idx != typeof[[]StrucOpt]().idx {
		assert false
	}
}
