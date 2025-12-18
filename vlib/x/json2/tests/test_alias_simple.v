import x.json2

type AliasSimpleTestStringAlias = string

struct StructWithAlias {
	name  string
	alias AliasSimpleTestStringAlias
}

fn main() {
	struct_with_alias := StructWithAlias{
		name: 'test'
		alias: 'alias_value'
	}
	
	result := json2.map_from(struct_with_alias)
	println('Result: ${result}')
	println('Keys: ${result.keys()}')
	
	if 'name' in result {
		println('✓ name field included')
	} else {
		println('✗ name field NOT included')
	}
	
	if 'alias' in result {
		println('✓ alias field included')
	} else {
		println('✗ alias field NOT included')
	}
}
