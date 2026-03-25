import arrays

// Regression test for https://github.com/vlang/v/issues/26760
// Interface values passed through variadic parameter chains must preserve
// their type tags and data pointers.

interface Value {}

fn process_params(params []Value) string {
	mut result := []string{}
	for i := 0; i < params.len; i++ {
		param := params[i]
		match param {
			string {
				result << 'string:${param}'
			}
			i32 {
				result << 'i32:${param}'
			}
			[]u8 {
				result << '[]u8:${param.len}'
			}
			else {
				result << 'unknown'
			}
		}
	}
	return result.join(', ')
}

struct Statement {
mut:
	handle int
}

struct Transaction {
mut:
	handle int
}

fn (mut stmt Statement) execute(params ...Value) !string {
	return process_params(params)
}

fn (stmt Statement) close() ! {}

fn (mut t Transaction) prepare(query string) !Statement {
	return Statement{ handle: 1 }
}

fn (mut t Transaction) execute(query string, params ...Value) !string {
	mut stmt := t.prepare(query)!
	result := stmt.execute(...params)!
	stmt.close()!
	return result
}

struct ListParams {
	email  ?string
	role   ?string
	offset i32
	fetch  i32
}

fn get_conditions(p ListParams) (string, []Value) {
	mut conditions := []string{}
	mut params := []Value{}

	if email := p.email {
		conditions = arrays.concat(conditions, 'email = ?')
		params = arrays.concat(params, email)
	}

	if role := p.role {
		conditions = arrays.concat(conditions, 'role = ?')
		params = arrays.concat(params, role)
	}

	return conditions.join(' AND '), params
}

fn test_variadic_interface_forwarding_with_match() ! {
	p := ListParams{
		email:  'info@peony.com'
		offset: 0
		fetch:  10
	}

	conditions, mut params := get_conditions(p)
	params = arrays.concat(params, p.offset, p.fetch)

	mut tx := Transaction{ handle: 1 }
	result := tx.execute('SELECT * FROM users WHERE ${conditions}', ...params)!
	assert result == 'string:info@peony.com, i32:0, i32:10', 'got: ${result}'
}

fn test_variadic_interface_forwarding_single_param() ! {
	p := ListParams{
		email:  'info@peony.com'
		offset: 0
		fetch:  10
	}

	conditions, params := get_conditions(p)

	mut tx := Transaction{ handle: 1 }
	result := tx.execute('SELECT COUNT(*) FROM users WHERE ${conditions}', ...params)!
	assert result == 'string:info@peony.com', 'got: ${result}'
}

fn test_variadic_interface_forwarding_with_byte_arrays() ! {
	user_id_bin := [u8(1), 2, 3, 4]
	email := 'info@peony.com'

	mut params := [Value(user_id_bin), email]

	mut tx := Transaction{ handle: 1 }
	result := tx.execute('INSERT INTO users', ...params)!
	assert result == '[]u8:4, string:info@peony.com', 'got: ${result}'
}

fn test_variadic_interface_forwarding_repeated() ! {
	for _ in 0 .. 100 {
		p := ListParams{
			email:  'info@peony.com'
			role:   'admin'
			offset: 5
			fetch:  20
		}

		conditions, mut params := get_conditions(p)
		params = arrays.concat(params, p.offset, p.fetch)

		mut tx := Transaction{ handle: 1 }
		result := tx.execute('SELECT * FROM users WHERE ${conditions}', ...params)!
		assert result == 'string:info@peony.com, string:admin, i32:5, i32:20', 'got: ${result}'
	}
}
