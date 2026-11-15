const migrations = [schema_v1, schema_v2]!
const schema_v1 = ['CREATE TABLE a (x)', 'CREATE TABLE b (y)']
const schema_v2 = ['CREATE TABLE c (z)']
const literal_migrations = [['a', 'b'], ['c']]!
const nested_migrations = [[['first'], ['second']]!, [['third'], ['fourth']]!]!

type Statements = []string

const aliased_migrations = [Statements(['alpha']), Statements(['beta'])]!

fn test_const_fixed_array_of_dynamic_arrays() {
	assert migrations.len == 2
	assert migrations[0] == schema_v1
	assert migrations[1] == schema_v2
	assert literal_migrations[0] == ['a', 'b']
	assert literal_migrations[1] == ['c']
	assert nested_migrations[0][1][0] == 'second'
	assert nested_migrations[1][0][0] == 'third'
	assert aliased_migrations[0][0] == 'alpha'
	assert aliased_migrations[1][0] == 'beta'
}
