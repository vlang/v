type Type = int
type RType = rune

fn test_map_key_alias() {
	mut m_int := {12: '12', 2: '2'}
	m_int[14] = '14'
	m_int[Type(15)] = '15'
	assert m_int.str() == "{12: '12', 2: '2', 14: '14', 15: '15'}"
	//// /// ///// //
	mut m_rune := {`a`: '12', `l`: '14'} 
	m_rune[`g`] = '12'
	m_rune[RType(`$`)] = '16'
	assert m_rune.str() == "{`a`: '12', `l`: '14', `g`: '12', `$`: '16'}"
}
