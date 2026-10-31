// `c_type` leaves a function type as its raw `fn_ptr:<signature>` key, which
// never equals the registered `_fn_ptr_*` typedef that the optional's payload
// side resolves to. Every `?Fn` return therefore looked like a payload
// mismatch, and that path silently degrades the return to `{.ok = false}`,
// dropping the value instead of returning it.
type AliasMsg = int

type AliasCmd = fn () AliasMsg

fn goto_line_cmd(line int) AliasCmd {
	return fn [line] () AliasMsg {
		return AliasMsg(line)
	}
}

fn command_for(name string) ?AliasCmd {
	if name == 'go' {
		return goto_line_cmd(5)
	}
	return none
}

fn test_optional_fn_alias_return_keeps_its_value() {
	cmd := command_for('go') or { panic('expected a command for `go`') }
	assert cmd() == AliasMsg(5)
	assert command_for('stay') == none
}
