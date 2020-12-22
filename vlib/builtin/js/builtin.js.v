module builtin

// used to generate JS throw statements.
pub fn js_throw(s any) {
	#throw (s instanceof Error ? s : new Error(s))
}
