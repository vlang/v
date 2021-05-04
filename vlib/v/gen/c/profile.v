module c

import v.ast

pub struct ProfileCounterMeta {
	fn_name   string
	vpc_name  string
	vpc_calls string
}

fn (mut g Gen) profile_fn(fn_decl ast.FnDecl) {
	if g.pref.profile_no_inline && fn_decl.attrs.contains('inline') {
		g.defer_profile_code = ''
		return
	}
	fn_name := fn_decl.name
	if fn_name.starts_with('time.vpc_now') {
		g.defer_profile_code = ''
	} else {
		measure_fn_name := if g.pref.os == .macos { 'time__vpc_now_darwin' } else { 'time__vpc_now' }
		fn_profile_counter_name := 'vpc_$g.last_fn_c_name'
		fn_profile_counter_name_calls := '${fn_profile_counter_name}_calls'
		g.writeln('')
		g.writeln('\tdouble _PROF_FN_START = ${measure_fn_name}(); $fn_profile_counter_name_calls++; // $fn_name')
		g.writeln('')
		g.defer_profile_code = '\t$fn_profile_counter_name += ${measure_fn_name}() - _PROF_FN_START;'
		g.pcs_declarations.writeln('double $fn_profile_counter_name = 0.0; u64 $fn_profile_counter_name_calls = 0;')
		g.pcs << ProfileCounterMeta{
			fn_name: g.last_fn_c_name
			vpc_name: fn_profile_counter_name
			vpc_calls: fn_profile_counter_name_calls
		}
	}
}

pub fn (mut g Gen) gen_vprint_profile_stats() {
	g.pcs_declarations.writeln('void vprint_profile_stats(){')
	fstring := '"%14llu %14.3fms %14.0fns %s \\n"'
	if g.pref.profile_file == '-' {
		for pc_meta in g.pcs {
			g.pcs_declarations.writeln('\tif ($pc_meta.vpc_calls) printf($fstring, $pc_meta.vpc_calls, $pc_meta.vpc_name/1000000.0, $pc_meta.vpc_name/$pc_meta.vpc_calls, "$pc_meta.fn_name" );')
		}
	} else {
		g.pcs_declarations.writeln('\tFILE * fp;')
		g.pcs_declarations.writeln('\tfp = fopen ("$g.pref.profile_file", "w+");')
		for pc_meta in g.pcs {
			g.pcs_declarations.writeln('\tif ($pc_meta.vpc_calls) fprintf(fp, $fstring, $pc_meta.vpc_calls, $pc_meta.vpc_name/1000000.0, $pc_meta.vpc_name/$pc_meta.vpc_calls, "$pc_meta.fn_name" );')
		}
		g.pcs_declarations.writeln('\tfclose(fp);')
	}
	g.pcs_declarations.writeln('}')
}
