// vtest retry: 3
import db.sqlite

struct Address {
	id        int @[primary; sql: serial]
	parent_id int
	create_at string
	update_at string
	street    string
	city      string
	state     string
	zip_code  string
	proximity string
}

@[table: 'RealStates']
struct RealState {
	id        string @[primary; sql_type: 'uuid']
	parent_id int
	name      string    @[sql_type: 'varchar(80)']
	cnpj      string    @[sql_type: 'varchar(14)']
	address   []Address @[fkey: 'parent_id']
}

@[table: 'Realtors']
struct Realtor {
	id         ?string   @[primary; sql_type: 'uuid']
	first_name string    @[sql_type: 'VARCHAR(30)']
	last_name  string    @[sql_type: 'VARCHAR(30)']
	creci      string    @[sql_type: 'VARCHAR(8)']
	cnpj       ?string   @[sql_type: 'VARCHAR(15)']
	cpf        ?string   @[sql_type: 'VARCHAR(12)']
	phone      string    @[sql_type: 'VARCHAR(15)']
	real_state RealState @[fkey: 'id']
}

fn test_orm_nested_struct() {
	mut db := sqlite.connect(':memory:')!

	data := Realtor{
		first_name: 'John'
		last_name:  'Doe'
		creci:      '12345678'
		cnpj:       '12345678901234'
		cpf:        '12345678901'
		phone:      '1234567890'
		real_state: RealState{
			name:    'Teste'
			cnpj:    '12345678901234'
			address: [
				Address{
					street:    'Teste'
					city:      'Teste'
					state:     'Teste'
					zip_code:  'Teste'
					proximity: 'Teste'
				},
			]
		}
	}

	sql db {
		create table Address
		create table RealState
		create table Realtor
	}!

	sql db {
		insert data into Realtor
	}!

	x1 := sql db {
		select from Address
	}!
	assert x1.str() == "[Address{
    id: 1
    parent_id: 0
    create_at: ''
    update_at: ''
    street: 'Teste'
    city: 'Teste'
    state: 'Teste'
    zip_code: 'Teste'
    proximity: 'Teste'
}]"

	x2 := sql db {
		select from RealState
	}!
	assert x2.str() == "[RealState{
    id: ''
    parent_id: 0
    name: 'Teste'
    cnpj: '12345678901234'
    address: [Address{
        id: 1
        parent_id: 0
        create_at: ''
        update_at: ''
        street: 'Teste'
        city: 'Teste'
        state: 'Teste'
        zip_code: 'Teste'
        proximity: 'Teste'
    }]
}]"

	x3 := sql db {
		select from Realtor
	}!

	// FIXME!
	// I think this result is not correct.
	assert x3.str() == "[Realtor{
    id: Option(none)
    first_name: 'John'
    last_name: 'Doe'
    creci: '12345678'
    cnpj: Option('12345678901234')
    cpf: Option('12345678901')
    phone: '1234567890'
    real_state: RealState{
        id: ''
        parent_id: 0
        name: ''
        cnpj: ''
        address: []
    }
}]"

	db.close()!
}
