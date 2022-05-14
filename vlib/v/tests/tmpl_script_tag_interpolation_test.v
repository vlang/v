module main

struct PlotData {
	dates            []string
	numerical_result []int
}

fn (pd PlotData) mydate() string {
	return pd.dates[0]
}

fn (pd PlotData) myint(idx int) int {
	return pd.numerical_result[idx]
}

fn test_template_interpolation_can_be_selectively_turned_on_in_script_tags() {
	benchmark_plot_data := PlotData{['2012-11-30', '2022-12-29'], [5, 6, 7, 1]}
	username := 'abcd'
	year := 2022
	text := $tmpl('tmpl/selective_interpolation_in_script_tag.html')
	dump(text)
	assert text.contains('Username: abcd')
	assert text.contains("var non_interpolated_labels = ['2012-11-30', '2022-12-29'];")
	assert text.contains('var non_interpolated_values = [5, 6, 7, 1];')
	assert text.contains("var real_labels = ['2012-11-30', '2022-12-29']; //V_TEMPLATE")
	assert text.contains('var real_values = [5, 6, 7, 1]; //V_TEMPLATE')
	assert text.contains('Year: 2022')
	assert text.contains('myint: 6')
	assert text.contains('mydate: 2012-11-30')
}
