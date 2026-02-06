struct My {
	a int
	b f64
	c string
}

fn test_comptime_match_for_field_type() {
	x := My{}

	mut result := ''

	$for f in x.fields {
		f_name := f.name
		$match f.typ {
			int {
				result += '${f_name}=int,'
			}
			f64 {
				result += '${f_name}=f64,'
			}
			string {
				result += '${f_name}=string,'
			}
			$else {
				result += '${f_name}=unknown,'
			}
		}
	}
	assert result == 'a=int,b=f64,c=string,'
}

fn test_comptime_match_for_field_type_reverse() {
	x := My{}
	a := 100

	mut result := ''

	$for f in x.fields {
		f_name := f.name
		$match $int {
			f.typ {
				result += '${f_name}=int,'
			}
		}
	}
	assert result == 'a=int,'
}
