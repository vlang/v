module builtin

// FunctionParam holds type information for function and/or method arguments.
pub struct FunctionParam {
pub:
	typ  int
	name string
}

// FunctionData holds information about a parsed function.
// Used by compile-time reflection ($for method in T.methods)
pub struct FunctionData {
pub:
	name        string
	location    string
	attrs       []string
	args        []FunctionParam
	return_type int
	typ         int
}
