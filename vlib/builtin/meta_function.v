module builtin

// FunctionParam holds type information for function and/or method arguments.
pub struct FunctionParam {
pub:
	typ  int
	name string
}

// FunctionData holds information about a parsed function.
pub struct FunctionData {
pub:
	name        string
	location    string
	attrs       []string     // legacy flattened attributes
	attributes  []VAttribute // structured attributes for `attrs`
	args        []FunctionParam
	return_type int
	typ         int
}
