module main

import arrays

fn gen_table_info(attribute_names []string, framework_platform map[string][]int) map[string]map[string]string {
	mut table := map[string]map[string]string{}

	// nanoseconds
	mut max_times := map[string]int{}
	mut ten_perc_max_times := map[string]int{}
	mut min_times := map[string]int{}
	mut ten_perc_min_times := map[string]int{}

	// bigger to calculate percent
	mut max := 0.0
	mut ten_perc_max := 0.0
	mut min := 0.0
	mut ten_perc_min := 0.0

	// percentes
	mut max_fast := map[string]int{}
	mut ten_perc_max_fast := map[string]int{}
	mut min_fast := map[string]int{}
	mut ten_perc_min_fast := map[string]int{}

	// nanoseconds
	for idx, name in attribute_names {
		// qtd. of values in 10 % of arrays
		ten_perc := int(framework_platform[name].len / 10)

		// get 10% highter
		mut min_ten_array := framework_platform[name].clone()
		min_ten_array.sort()
		min_ten_array.trim(ten_perc)

		// get 10% lower
		mut max_ten_array := framework_platform[name].clone()
		max_ten_array.sort(a > b)
		max_ten_array.trim(ten_perc)

		// popule array with nanoseconds to which benchmark
		max_times[name] = arrays.max(framework_platform[name]) or { 0 } // int
		ten_perc_max_times[name] = arrays.sum(max_ten_array) or { 0 } / ten_perc // int
		min_times[name] = arrays.min(framework_platform[name]) or { 0 } // int
		ten_perc_min_times[name] = arrays.sum(min_ten_array) or { 0 } / ten_perc // int

		// set bigger values
		if idx < 1 {
			max = f64(max_times[name])
			ten_perc_max = f64(ten_perc_max_times[name])
			min = f64(min_times[name])
			ten_perc_min = f64(ten_perc_min_times[name])
		} else {
			if max < f64(max_times[name]) {
				max = f64(max_times[name])
			}
			if ten_perc_max < f64(ten_perc_max_times[name]) {
				ten_perc_max = f64(ten_perc_max_times[name])
			}
			if min < f64(min_times[name]) {
				min = f64(min_times[name])
			}
			if ten_perc_min < f64(ten_perc_min_times[name]) {
				ten_perc_min = f64(ten_perc_min_times[name])
			}
		}
	}

	// percents
	for name in attribute_names {
		max_fast[name] = int(max / f64(max_times[name]))
		ten_perc_max_fast[name] = int(ten_perc_max / f64(ten_perc_max_times[name]))
		min_fast[name] = int(min / f64(min_times[name]))
		ten_perc_min_fast[name] = int(ten_perc_min / f64(ten_perc_min_times[name]))
		// max_fast[name] = int(100 - (f64(max_times[name] * 100) / max))
		// ten_perc_max_fast[name] = int(100 - (f64(ten_perc_max_times[name] * 100) / ten_perc_max))
		// min_fast[name] = int(100 - (f64(min_times[name] * 100) / min))
		// ten_perc_min_fast[name] = int(100 - (f64(ten_perc_min_times[name] * 100) / ten_perc_min))
	}

	for name in attribute_names {
		table[name]['max.'] = '${max_times[name]} ns (${max_fast[name]}x faster)'
		table[name]['10% max.'] = '${ten_perc_max_times[name]} ns (${ten_perc_max_fast[name]}x faster)'
		table[name]['min.'] = '${min_times[name]} ns (${min_fast[name]}x faster)'
		table[name]['10% min.'] = '${ten_perc_min_times[name]} ns (${ten_perc_min_fast[name]}x faster)'
	}
	return table
}
