module transform

// is_generic_fn checks if a function name refers to a generic function.
// Generic functions have type parameters that need to be monomorphized
// into concrete specializations before code generation.
// For now, returns false as generics are not yet supported in v3.
fn (mut t Transformer) is_generic_fn(_name string) bool {
	return false
}

// is_generic_struct checks if a struct name is generic.
// Generic structs like `Array[T]` would need monomorphization into
// concrete types like `Array_int`, `Array_string`, etc.
// For now, returns false as generics are not yet supported in v3.
fn (mut t Transformer) is_generic_struct(_name string) bool {
	return false
}

// monomorphize_pass is the top-level generic monomorphization pass.
// When generics support is added, this will:
//   1. Scan all call sites and struct inits for generic instantiations
//   2. For each unique set of type arguments, clone and specialize the
//      generic function/struct definition
//   3. Rewrite call sites to reference the monomorphized specialization
//   4. Remove the original generic definitions
// Called from transform_all() in transform.v if generics support is added.
fn (mut t Transformer) monomorphize_pass() {
}
