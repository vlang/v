// Regression test: when a sum type (or interface) whose shared field is named
// after a C/C++ reserved word (`operator`, `explicit`, ...) is cast to a wider
// sum type, write_sumtype_casting_fn must escape the field on *both* sides of the
// generated assignment. Otherwise the source access is emitted as `ptr->operator`
// while both type declarations name the member `__v_operator`, so the generated C
// references a nonexistent member (and is invalid C++ for `operator`).
// See write_sumtype_casting_fn in vlib/v/gen/c/cgen.v.
module main

struct ReservedAlpha {
	operator int
	explicit int
}

struct ReservedBeta {
	operator int
	explicit int
}

struct ReservedGamma {
	operator int
	explicit int
}

type ReservedInner = ReservedAlpha | ReservedBeta

// ReservedInner is itself a variant of ReservedOuter, so casting an inner value
// to the outer sum type goes through the `got_sym.kind in [.sum_type, .interface]`
// branch, with `operator`/`explicit` as shared fields.
type ReservedOuter = ReservedGamma | ReservedInner

fn test_sumtype_cast_preserves_reserved_common_fields() {
	inner := ReservedInner(ReservedAlpha{
		operator: 42
		explicit: 7
	})
	outer := ReservedOuter(inner)
	assert outer.operator == 42
	assert outer.explicit == 7
}
