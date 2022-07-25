# ORM

## Attributes

### Structs

- `[table: 'name']` sets a custom table name

### Fields

- `[primary]` sets the field as the primary key
- `[unique]` sets the field as unique
- `[unique: 'foo']` adds the field to a unique group
- `[skip]` field will be skipped
- `[sql: type]` where `type` is a V type such as `int` or `f64`, or special type `serial`
- `[sql: 'name']` sets a custom column name for the field
- `[sql_type: 'SQL TYPE']` sets the sql type which is used in sql
- `[default: 'sql defaults']` sets the default value or function when create a new table

## Usage

```v ignore
struct Foo {
    id          int         [primary; sql: serial]
    name        string      [nonull]
    created_at  time.Time   [sql_type: 'DATETIME']
    updated_at  string      [sql_type: 'DATETIME']
    deleted_at  time.Time
}
```

### Create

```v ignore
sql db {
    create table Foo
}
```

### Drop

```v ignore
sql db {
    drop table Foo
}
```

### Insert

```v ignore
var := Foo{
    name:       'abc'
    created_at: time.now()
    updated_at: time.now().str()
    deleted_at: time.now()
}

sql db {
    insert var into Foo
}
```

### Update

```v ignore
sql db {
    update Foo set name = 'cde' where name == 'abc'
}
```

### Delete
```v ignore
sql db {
    delete from Foo where id > 10
}
```

### Select
```v ignore
result := sql db {
    select from Foo where id == 1
}
```
```v ignore
result := sql db {
    select from Foo where id > 1 limit 5
}
```
```v ignore
result := sql db {
    select from Foo where id > 1 order by id
}
```

### Example
```v ignore
import pg

struct Member {
	id         string [default: 'gen_random_uuid()'; primary; sql_type: 'uuid']
	name       string
	created_at string [default: 'CURRENT_TIMESTAMP'; sql_type: 'TIMESTAMP']
}

fn main() {
	db := pg.connect(pg.Config{
		host: 'localhost'
		port: 5432
		user: 'user'
		password: 'password'
		dbname: 'dbname'
	}) or {
		println(err)
		return
	}

	defer {
		db.close()
	}

	sql db {
		create table Member
	}

	new_member := Member{
		name: 'John Doe'
	}

	sql db {
		insert new_member into Member
	}

	selected_member := sql db {
		select from Member where name == 'John Doe' limit 1
	}

	sql db {
		update Member set name = 'Hitalo' where id == selected_member.id
	}
}


```