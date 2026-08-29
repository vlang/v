module fastc

// reject_anonymous_function rejects a function literal before any of its body is
// discarded. FastC has no closure runtime, so emitting a callable stub would silently
// change program behavior.
fn (mut g Parser) reject_anonymous_function() !string {
	return g.unsupported('function literals and closures')
}
