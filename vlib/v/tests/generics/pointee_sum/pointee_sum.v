module pointee_sum

// Number has the pointee of `Maybe[&int]` as a variant, see
// generic_sumtype_pointer_variant_test.v.
pub type Number = int | f64
