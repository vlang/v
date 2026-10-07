// `#line N "file"` makes the source line after it report line N of `file`, like in C.
struct Generated {}

#line 10 "gen.zbr"
fn (g Generated) location() string {
	#line 20 "src/app.zbr"
	return @LOCATION
}

fn test_line_directive_remaps_pseudo_variables() {
	#line 42 "src/app.zbr"
	assert @FILE == 'src/app.zbr'
	assert @LINE == '43'
	assert @FILE_LINE == 'app.zbr:44'
	assert Generated{}.location() == 'src/app.zbr:20, main.Generated{}.location'
	// `#line N` keeps the logical file.
	#line 100
	assert @LINE == '100'
	assert @FILE == 'src/app.zbr'
	// The column stays the one in the V source.
	assert @COLUMN == '9'
}

fn test_line_directive_applies_to_the_rest_of_the_file() {
	assert @FILE == 'src/app.zbr'
	#line 7 "other.zbr"
	assert @FILE_LINE == 'other.zbr:7'
}

fn test_line_directive_remaps_method_locations() {
	$for method in Generated.methods {
		assert method.location.starts_with('gen.zbr:10:')
	}
}
