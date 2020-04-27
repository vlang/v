module gen

pub struct ProfileCounterMeta{
	fn_name string
	vpc_name string
	vpc_calls string
}

fn (mut g Gen) profile_fn(fn_name string, is_main bool){
	if is_main {
		g.writeln('')
		g.writeln('\tatexit(vprint_profile_stats);')
		g.writeln('')
	}
	if fn_name == 'time.vpc_now' {
		g.defer_profile_code = ''
	} else {
		fn_profile_counter_name := 'vpc_${g.last_fn_c_name}'
		fn_profile_counter_name_calls := '${fn_profile_counter_name}_calls'
		g.writeln('')
		g.writeln('\tdouble _PROF_FN_START = time__vpc_now(); ${fn_profile_counter_name_calls}++; // $fn_name')
		g.writeln('')
		g.defer_profile_code = '\t${fn_profile_counter_name} += time__vpc_now() - _PROF_FN_START;'
		g.pcs_declarations.writeln('double ${fn_profile_counter_name} = 0.0; u64 ${fn_profile_counter_name_calls} = 0;')
		g.pcs << ProfileCounterMeta{ g.last_fn_c_name, fn_profile_counter_name, fn_profile_counter_name_calls }
	}
}

        
pub fn (mut g Gen) gen_vprint_profile_stats() {
	g.pcs_declarations.writeln('void vprint_profile_stats(){')
	if g.pref.profile_file == '-' {
		for pc_meta in g.pcs {
			g.pcs_declarations.writeln('\tif (${pc_meta.vpc_calls}) printf("%llu %f %f ${pc_meta.fn_name} \\n", ${pc_meta.vpc_calls}, ${pc_meta.vpc_name}, ${pc_meta.vpc_name}/${pc_meta.vpc_calls} );')
		}
	} else {
		g.pcs_declarations.writeln('\tFILE * fp;')
		g.pcs_declarations.writeln('\tfp = fopen ("${g.pref.profile_file}", "w+");')
		for pc_meta in g.pcs {
			g.pcs_declarations.writeln('\tif (${pc_meta.vpc_calls}) fprintf(fp, "%llu %f %f ${pc_meta.fn_name} \\n", ${pc_meta.vpc_calls}, ${pc_meta.vpc_name}, ${pc_meta.vpc_name}/${pc_meta.vpc_calls} );')
		}
		g.pcs_declarations.writeln('\tfclose(fp);')
	}
	g.pcs_declarations.writeln('}')
}
