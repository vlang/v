module fastc

// read_anonymous_function_stub parses a function literal / closure
// (`fn [captures] (params) ret { body }`) in expression position and lowers it to a
// COMPILE-ONLY stub: a top-level function of the matching signature whose body is
// dropped. This lets code that stores or passes a closure value (e.g. net/http's H2
// transport `close_transport`, which is captured and invoked later) compile and link.
// FastC has no closure runtime — capturing the enclosing environment through an escaping
// function pointer needs per-arch thunks — so the stub is non-functional, mirroring the
// channel stubs. Returns `&name` (the stub's address, a function-pointer value) so a
// `:=` declaration infers a proper C function-pointer type via `__typeof__`.
fn (mut g Parser) read_anonymous_function_stub() !string {
	g.next() // consume `fn`
	// Skip an optional capture list `[a, mut b, ...]`.
	if g.tok == .lsbr {
		g.skip_balanced(.lsbr, .rsbr)!
	}
	// Parse the parameters for the stub signature. parse_parameters registers the params
	// as locals; isolate them so the enclosing scope is unaffected.
	saved_locals := g.locals.clone()
	g.expect(.lpar)!
	params := g.parse_parameters()!
	mut return_type := 'void'
	if g.tok != .lcbr && g.tok != .semicolon {
		if g.tok in [.not, .question] {
			g.next()
			return_type = 'Option'
			if g.tok in [.lcbr, .semicolon] {
			} else if g.tok == .lpar {
				_ := g.parse_multi_return_types()!
			} else {
				_ := g.parse_type()!
			}
		} else if g.tok == .lpar {
			_ := g.parse_multi_return_types()!
			return_type = 'MultiReturn'
		} else {
			return_type = g.parse_type()!
		}
	}
	g.locals = saved_locals.clone()
	// Drop the body.
	g.skip_balanced(.lcbr, .rcbr)!
	// Emit a uniquely-named stub function. The name folds in the enclosing
	// module/receiver/function (unique per definition) plus a per-file counter, so two
	// closures anywhere in the program never collide in the shared helper map.
	g.anon_fn_counter++
	stem :=
		fastc_c_function_name_for_key('${g.module_name}.${g.current_receiver}.${g.current_function}')
	name := '${stem}__anonfn_${g.anon_fn_counter}'
	c_params := if params.len == 0 { 'void' } else { params.join(', ') }
	body_return := if return_type == 'void' {
		''
	} else if return_type.ends_with('*') {
		'return 0;'
	} else {
		'return (${return_type}){0};'
	}
	g.protos.writeln('${return_type} ${name}(${c_params});')
	g.spawn_helpers[name] = '${return_type} ${name}(${c_params}) { ${body_return} }'
	return '&${name}'
}
