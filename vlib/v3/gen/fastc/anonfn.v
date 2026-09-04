module fastc

import os
import v3.scanner
import v3.token

// parse_anonymous_function lowers a non-capturing anonymous function literal
// (`fn (params) rettype { body }`) to a hoisted top-level C function and returns
// its name, which decays to a function pointer at the call site. FastC has no
// closure runtime, so a capture list (`fn [x] () {...}`) is still rejected.
//
// The literal is re-parsed as an ordinary named function via `parse_function`,
// reusing the same machinery the on-demand monomorphization drain relies on. Its
// definition is captured out of `g.out` and stored alongside the mono instances
// so it is emitted at top level; the enclosing expression only keeps the name.
fn (mut g Parser) parse_anonymous_function() !string {
	// `fn [x] (...) {...}` explicitly captures locals: a real closure, which FastC
	// cannot represent without a runtime, so reject it before touching the body.
	mut capture_look := g.s
	if capture_look.scan() == .lsbr {
		return g.unsupported('function literals and closures')
	}
	anon_start := g.s.pos
	// Advance the main scanner past the whole literal: `fn`, an optional parameter
	// list, the return type, and the balanced body block. `body_end` marks the byte
	// just past the closing `}`.
	g.next() // consume `fn`
	if g.tok == .lpar {
		g.skip_balanced(.lpar, .rpar)!
	}
	for g.tok != .lcbr && g.tok != .eof {
		g.next()
	}
	if g.tok != .lcbr {
		return g.unsupported('anonymous function body')
	}
	mut depth := 0
	mut body_end := 0
	for {
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			depth--
			body_end = g.s.offset
			g.next()
			if depth == 0 {
				break
			}
			continue
		} else if g.tok == .eof {
			return g.unsupported('unfinished anonymous function body')
		}
		g.next()
	}
	anon_source := g.s.src[anon_start..body_end]
	name := '__v_fastc_anon_${g.module_name.replace('.', '_')}_${os.file_name(g.path).replace('.', '_')}_${anon_start}'
	// Rewrite `fn (...) ...` as `fn <name> (...) ...` so `parse_function` reads it as
	// an ordinary declaration.
	after_fn_keyword := anon_source[2..]
	synthetic := 'fn ${name} ${after_fn_keyword}'
	c_name := fastc_c_function_name(g.module_name, name)

	// Save the resume position (already advanced past the literal) and every piece of
	// parser state `parse_function` mutates but does not itself restore.
	resume_s := g.s
	resume_tok := g.tok
	resume_lit := g.lit
	saved_locals := g.locals.clone()
	saved_temp_id := g.temp_id
	saved_indent := g.indent
	saved_scope_changes := g.local_scope_changes.clone()
	saved_scope_depth := g.local_scope_depth
	saved_drain := g.in_mono_drain

	start := g.out.len
	g.indent = 0
	// Emit the hoisted function even though its synthetic name is not in the reachable
	// set, exactly as the mono drain does for on-demand instances.
	g.in_mono_drain = true
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(g.path, synthetic.len)
	file.index_lines_without_digest(synthetic)
	g.s = scanner.new_scanner(g.prefs, .normal)
	g.s.init(file, synthetic)
	g.next()
	g.parse_function(true) or {
		// Restore before propagating so the outer parse can report cleanly.
		g.s = resume_s
		g.tok = resume_tok
		g.lit = resume_lit
		g.locals = saved_locals.clone()
		g.temp_id = saved_temp_id
		g.indent = saved_indent
		g.local_scope_changes = saved_scope_changes.clone()
		g.local_scope_depth = saved_scope_depth
		g.in_mono_drain = saved_drain
		g.out.go_back(g.out.len - start)
		return err
	}
	definition := g.out.after(start)
	g.out.go_back(g.out.len - start)
	if definition != '' {
		g.generated_mono[c_name] = true
		g.mono_definitions[c_name] = definition
	}

	// Restore the enclosing parse and resume just past the literal.
	g.s = resume_s
	g.tok = resume_tok
	g.lit = resume_lit
	g.locals = saved_locals.clone()
	g.temp_id = saved_temp_id
	g.indent = saved_indent
	g.local_scope_changes = saved_scope_changes.clone()
	g.local_scope_depth = saved_scope_depth
	g.in_mono_drain = saved_drain
	return c_name
}
