# ORM

## Null

Use option fields in V structs for fields which can be NULL.  Regular,
non-option fields are defied as NOT NULL when creating tables.

## Attributes

### Structs

- `[table: 'name']` sets a custom table name

### Fields

- `[primary]` sets the field as the primary key
- `[unique]` sets the field as unique
- `[unique: 'foo']` adds the field to a unique group
- `[skip]` or `[sql: '-']` field will be skipped
- `[sql: type]` where `type` is a V type such as `int` or `f64`
- `[sql: serial]` lets the DB backend choose a column type for a auto-increment field
- `[sql: 'name']` sets a custom column name for the field
- `[sql_type: 'SQL TYPE']` sets the sql type which is used in sql
- `[default: 'raw_sql]` inserts `raw_sql` verbatim in a "DEFAULT" clause when
  create a new table, allowing for values like `CURRENT_TIME`
- `[fkey: 'parent_id']` sets foreign key for an field which holds an array

## Usage

```v ignore
import time

[table: 'foos']
struct Foo {
    id          int         [primary; sql: serial]
    name        string
    created_at  time.Time   [default: 'CURRENT_TIME]
    updated_at  ?string     [sql_type: 'TIMESTAMP]
    deleted_at  ?time.Time
    children    []Child     [fkey: 'parent_id']
}

struct Child {
    id        int    [primary; sql: serial]
    parent_id int
    name      string
}
```

### Create

```v ignore
sql db {
    create table Foo
}!
```

### Drop

```v ignore
sql db {
    drop table Foo
}!
```

### Insert

```v ignore
foo := Foo{
    name:       'abc'
    created_at: time.now()
    // updated_at defaults to none
    // deleted_at defaults to none
    children: [
        Child{
            name: 'abc'
        },
        Child{
            name: 'def'
        },
    ]
}

sql db {
    insert foo into Foo
}!
```

When inserting, `[sql: serial]` fields, and fields with a `[default: 'raw_sql']`
attribute are not sent to the database when the value being sent is the default
for the V struct field (e.g., 0 int, or an empty string).  This allows the
database to insert default values for auto-increment fields and where you have
specified a default.

### Update

```v ignore
sql db {
    update Foo set updated_at = time.now() where name == 'abc' && updated_at is none
}!
```

Note that `is none` and `!is none` can be used to select for NULL fields.

### Delete
```v ignore
sql db {
    delete from Foo where id > 10
}!
```

### Select
```v ignore
result := sql db {
    select from Foo where id == 1
}!
```
```v ignore
result := sql db {
    select from Foo where id > 1 && name != 'lasanha' limit 5
}!
```
```v ignore
result := sql db {
    select from Foo where id > 1 order by id
}!
```

### Example
```v ignore
import db.pg

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
	})!

	defer {
		db.close()
	}

	sql db {
		create table Member
	}!

	new_member := Member{
		name: 'John Doe'
	}

	sql db {
		insert new_member into Member
	}!

	selected_members := sql db {
		select from Member where name == 'John Doe' limit 1
	}!
	john_doe := selected_members.first()

	sql db {
		update Member set name = 'Hitalo' where id == john_doe.id
	}!
}
```
