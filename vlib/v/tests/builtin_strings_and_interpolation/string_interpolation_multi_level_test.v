fn foo() ?string {
	return none
}

fn test_string_interpolation_multi_level() {
	x := 0

	assert '${if x == 0 {
		'${x}'
	} else {
		'${'${x + 1}'}'
	}}' == '0'

	assert '${foo() or { '${if x == 0 {
		'${x}'
	} else {
		'${x + 1}'
	}}' }}' == '0'

	println('${match true {
		true { '${x}' }
		else { '${x + 1}' }
	}}' == '0')

	// codegen error
	// assert '${match true { true { '${x}' } else { '${x + 1}' } }}' == '0'

	assert '${if x == 0 {
		'${match x {
			1 { x == 1 }
			else { x != 0 }
		}}'
	} else {
		''
	}}' == 'false'

	assert '${if x == 0 {
		'${if x == 1 {
			'${if x == 2 {
				'${x + 2}'
			} else {
				'${1}'
			}}'
		} else {
			'${if x == 0 {
				'${match x {
					2 {
						'true'
					}
					else {
						'${x + 3}'
					}
				}}'
			} else {
				'${false}'
			}}'
		}}'
	} else {
		''
	}}' == '3'

	assert '${if m := foo() {
		'${m}'
	} else {
		'${x + 1}'
	}}' == '1'
}
