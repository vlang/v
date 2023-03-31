pub struct ParamPrecos {
pub:
	codigo            string [required]
	tabela_referencia ?i64
}

fn test_none() {
	pp := ParamPrecos{
		codigo: 'opa'
	}

	tx_ref := match pp.tabela_referencia {
		none { 'num: none' }
		else { 'num: ${pp.tabela_referencia?}' }
	}
	assert tx_ref == 'num: none'
}

fn test_not_none() {
	pp := ParamPrecos{
		codigo: 'opa'
		tabela_referencia: 123
	}

	tx_ref := match pp.tabela_referencia {
		none { 'num: none' }
		else { 'num: ${pp.tabela_referencia?}' }
	}
	assert tx_ref == 'num: 123'
}
