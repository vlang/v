pub struct ParamPrecos {
pub:
	code      string @[required]
	table_ref ?i64
}

fn test_none() {
	pp := ParamPrecos{
		code: 'V'
	}

	tx_ref := match pp.table_ref {
		none { 'num: none' }
		else { 'num: ${pp.table_ref?}' }
	}
	assert tx_ref == 'num: none'
}

fn test_not_none() {
	pp := ParamPrecos{
		code:      'V'
		table_ref: 123
	}

	tx_ref := match pp.table_ref {
		none { 'num: none' }
		else { 'num: ${pp.table_ref?}' }
	}
	assert tx_ref == 'num: 123'
}
