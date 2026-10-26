module transform

import v.flat
import v.types

fn used_name_reference(mut t Transformer, name string) {
	if name == '' {
		return
	}
	lowered := if isnil(t.tc) { c_name(name) } else { t.tc.cached_c_name(name) }
	if !t.used_fn_contains_name(name) && !t.used_fn_contains_name(lowered)
		&& t.enum_method_name_shadows_field(name) {
		return
	}
	t.mark_used_fn_key(name)
	t.mark_used_fn_key(lowered)
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		needs_module_prefix := !name.contains('.') || local_method_fn_name_needs_module_prefix(name)
		if needs_module_prefix && !name.starts_with('${t.cur_module}.') {
			qname := '${t.cur_module}.${name}'
			t.mark_used_fn_key(qname)
			t.mark_used_fn_key(if isnil(t.tc) { c_name(qname) } else { t.tc.cached_c_name(qname) })
		}
	}
}

fn test_used_name_probes_preserve_spellings_overlays_and_log_order() {
	for module_name in ['', 'main', 'builtin', 'demo'] {
		for logging in [false, true] {
			for state in 0 .. 8 {
				mut a := flat.FlatAst.new()
				mut tc := types.TypeChecker.new(&a)
				mut slow := new_transformer(mut a, &tc, map[string]bool{})
				mut fast := new_transformer(mut a, &tc, map[string]bool{})
				mut parent := map[string]bool{}
				mut root := map[string]bool{}
				if state & 1 != 0 {
					parent['demo.call'] = true
					parent['helper'] = true
				}
				if state & 2 != 0 {
					root['demo__call'] = true
					root['Color.red'] = true
				}
				if state & 4 != 0 {
					slow.used_fns['malloc'] = true
					fast.used_fns['malloc'] = true
				}
				slow.used_fns_parent = unsafe { &parent }
				fast.used_fns_parent = unsafe { &parent }
				slow.used_fns_root = unsafe { &root }
				fast.used_fns_root = unsafe { &root }
				slow.cur_module = module_name
				fast.cur_module = module_name
				slow.used_fns_log_active = logging
				fast.used_fns_log_active = logging
				slow.enum_types['Color'] = ['red', 'blue']
				fast.enum_types['Color'] = ['red', 'blue']
				for name in ['', 'helper', 'demo.call', 'malloc', 'int_str', 'exit', 'Color.red',
					'Color.method', 'word__value', 'Box.+', 'helper'] {
					used_name_reference(mut slow, name)
					fast.mark_fn_used_name(name)
					assert fast.used_fns == slow.used_fns, '${module_name}:${state}:${name}'
					assert fast.used_fns_log == slow.used_fns_log, '${module_name}:${state}:${name}'
				}
				// A parent's additions remain visible on later invocations.
				parent['later'] = true
				used_name_reference(mut slow, 'later')
				fast.mark_fn_used_name('later')
				assert fast.used_fns == slow.used_fns
				assert fast.used_fns_log == slow.used_fns_log
			}
		}
	}
}
