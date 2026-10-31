module types

import v.flat

// smartcast_type_name returns the source spelling ID of the type `id` was narrowed to by an enclosing
// `is` (or `!= none`) check, or zero when the expression is not smartcast.
// The transform lowers comparisons after checking, and has to see the same narrowed
// type the checker validated the expression with: an index or selector node keeps
// its declared type, so without this a smartcast sum type operand would still be
// compared with the sum type's equality helper.
pub fn (tc &TypeChecker) smartcast_type_name(id flat.NodeId) u32 {
	typ := tc.smartcast_type(id) or { return u32(0) }
	return tc.type_name(typ)
}
