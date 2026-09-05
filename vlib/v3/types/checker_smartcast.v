module types

import v3.flat

// smartcast_type_name is the name of the type `id` was narrowed to by an enclosing
// `is` (or `!= none`) check, or '' when the expression is not smartcast.
// The transform lowers comparisons after checking, and has to see the same narrowed
// type the checker validated the expression with: an index or selector node keeps
// its declared type, so without this a smartcast sum type operand would still be
// compared with the sum type's equality helper.
pub fn (tc &TypeChecker) smartcast_type_name(id flat.NodeId) string {
	typ := tc.smartcast_type(id) or { return '' }
	return tc.type_name(typ)
}
