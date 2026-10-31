@[has_globals; translated]
module main

type Finder = fn (int) int

fn plus_one(x int) int {
	return x + 1
}

__global finder = Finder(plus_one)

struct Vfs {
	app voidptr
}

struct Holder {
	f Finder = plus_one
}

// In translated C, `voidptr(&f)` of a function variable is the address of the
// variable, which can be read back as a function (SQLite's unix VFS stores
// `(void*)&finder` and calls `(**(finder_type*)pAppData)(...)`). `&` on a
// function name is the function itself.
fn test_address_of_a_function_variable_as_voidptr() {
	vfs := Vfs{
		app: voidptr(&finder)
	}
	from_global := unsafe { *&Finder(vfs.app) }
	assert from_global(41) == 42
	local := Finder(plus_one)
	p := voidptr(&local)
	from_local := unsafe { *&Finder(p) }
	assert from_local(1) == 2
	holder := Holder{}
	q := voidptr(&holder.f)
	from_field := unsafe { *&Finder(q) }
	assert from_field(2) == 3
	assert voidptr(&plus_one) == voidptr(plus_one)
}

fn assign_voidptr(target &voidptr, value voidptr) voidptr {
	unsafe {
		*target = value
	}
	return value
}

// C: `if ((x = pVtab->pModule->xSync) != 0) rc = x(pVtab);`, translated by c2v.
fn test_a_function_stored_through_the_address_of_a_local() {
	holder := Holder{}
	x := Finder(unsafe { nil })
	if !isnil(Finder(assign_voidptr(unsafe { &voidptr(&x) }, voidptr(holder.f)))) {
		assert x(41) == 42
	} else {
		assert false
	}
}
