module c

import v.flat
import v.pref
import v.types

fn test_profile_report_uses_v_only_for_raw_macos_amd64() {
	for arch in ['amd64', 'arm64'] {
		for bare in [false, true] {
			for output in ['-', 'report.txt'] {
				mut a := flat.FlatAst.new()
				mut tc := types.TypeChecker.new(&a)
				mut g := FlatGen.new()
				g.a = &a
				g.tc = &tc
				g.target = pref.target_from('macos', arch) or { panic(err) }
				g.set_profile(output, false, [])
				if bare {
					g.set_compile_values({
						'freestanding': '1'
					})
				}
				g.profile_counters = [ProfileCounterMeta{'main__main', 'counter', 'calls'}]
				g.gen_profile_support()
				code := g.sb.str()
				if bare && arch == 'amd64' {
					assert code.contains('profile__begin_report('), code
					assert code.contains('profile__write_report_row(&_v_profile_report, calls, counter, counter_only_current,'), code
					assert code.contains('profile__end_report(&_v_profile_report);'), code
					for symbol in ['fopen(', 'fprintf(', 'printf(', 'fclose(', 'FILE*'] {
						assert !code.contains(symbol), code
					}
				} else {
					assert !code.contains('profile__begin_report'), code
					assert code.contains(if output == '-' { 'printf(' } else { 'fopen(' }), code
				}
				assert code.contains('calls = 0;') && code.contains('counter_only_current = 0.0;'), code
				g.sb.clear()
				g.gen_profile_registration()
				registration := g.sb.str()
				assert registration.contains('v_signal_with_handler_cast(SIGINT,'), registration
				assert registration.contains('v_signal_with_handler_cast(SIGTERM,'), registration
				assert registration.contains('atexit(vprint_profile_stats);'), registration
			}
		}
	}
}

fn test_raw_profile_report_suspends_nested_entry_tracing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.target = pref.target_from('macos', 'amd64') or { panic(err) }
	g.set_compile_values({
		'freestanding': '1'
	})
	g.set_profile('-', false, [])
	g.set_trace_calls(true, [])
	g.gen_profile_support()
	code := g.sb.str()
	disabled := code.index('g_trace_in_hook = true;') or { panic(code) }
	begin := code.index('profile__begin_report(') or { panic(code) }
	end := code.index('profile__end_report(') or { panic(code) }
	restored := code.index('g_trace_in_hook = _v_profile_previous_trace;') or { panic(code) }
	assert disabled < begin && end < restored, code
}
