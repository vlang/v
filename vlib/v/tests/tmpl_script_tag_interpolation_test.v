module main

struct PlotData {
	dates []string
	numerical_result []int
}
	
fn test_template_interpolation_can_be_selectively_turned_on_in_script_tags() {
	benchmark_plot_data := PlotData{['2012-11-30', '2022-12-29'], [5,6,7,1]}
	text := $tmpl('tmpl/selective_interpolation_in_script_tag.html')
	// dump(text)
	assert text.contains("var non_interpolated_labels = @benchmark_plot_data.dates;")
	assert text.contains("var non_interpolated_values = @benchmark_plot_data.numerical_result;")
	assert text.contains("var real_labels = ['2012-11-30', '2022-12-29']; //V_TEMPLATE")
	assert text.contains("var real_values = [5, 6, 7, 1]; //V_TEMPLATE")
}
