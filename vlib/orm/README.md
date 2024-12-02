# ORM

V has a powerful, concise ORM baked in! Create tables, insert records, manage relationships, all
regardless of the DB driver you decide to use.

## Nullable

For a nullable column, use an option field. If the field is non-option, the column will be defined
with `NOT NULL` at table creation.

```v ignore
struct Foo {
    notnull  string
    nullable ?string
}
```

## Attributes

### Structs

- `[table: 'name']` explicitly sets the name of the table for the struct

### Fields

- `[primary]` sets the field as the primary key
- `[unique]` gives the field a `UNIQUE` constraint
- `[unique: 'foo']` adds the field to a `UNIQUE` group
- `[skip]` or `[sql: '-']` field will be skipped
- `[sql: type]` where `type` is a V type such as `int` or `f64`
- `[serial]` or `[sql: serial]` lets the DB backend choose a column type for an auto-increment field
- `[sql: 'name']` sets a custom column name for the field
- `[sql_type: 'SQL TYPE']` explicitly sets the type in SQL
- `[default: 'raw_sql']` inserts `raw_sql` verbatim in a "DEFAULT" clause when
  creating a new table, allowing for SQL functions like `CURRENT_TIME`. For raw strings, 
  surround `raw_sql` with backticks (\`).

- `[fkey: 'parent_id']` sets foreign key for an field which holds an array

## Usage

Here are a couple example structs showing most of the features outlined above.

```v ignore
import time

@[table: 'foos']
struct Foo {
    id          int         @[primary; sql: serial]
    name        string
    created_at  time.Time   @[default: 'CURRENT_TIME']
    updated_at  ?string     @[sql_type: 'TIMESTAMP']
    deleted_at  ?time.Time
    children    []Child     @[fkey: 'parent_id']
}

struct Child {
    id        int    @[primary; sql: serial]
    parent_id int
    name      string
}
```

To use the ORM, there is a special interface that lets you use the structs and V itself in queries.
This interface takes the database instance as an argument.

```v ignore
import db.sqlite

db := sqlite.connect(':memory:')!

sql db {
    // query; see below
}!
```

When you need to reference the table, simply pass the struct itself.

```v ignore
import models.Foo

struct Bar {
    id int @[primary; sql: serial]
}

sql db {
    create table models.Foo
    create table Bar
}!
```

### Create & Drop Tables

You can create and drop tables by passing the struct to `create table` and `drop table`.

```v ignore
import models.Foo

struct Bar {
    id int @[primary; sql: serial]
}

sql db {
    create table models.Foo
    drop table Bar
}!
```

### Insert Records

To insert a record, create a struct and pass the variable to the query. Again, reference the struct
as the table.

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

foo_id := sql db {
    insert foo into Foo
}!
```

If the `id` field is marked as `sql: serial` and `primary`, the insert expression
returns the database ID of the newly added object. Getting an ID of a newly
added DB row is often useful.

When inserting, `[sql: serial]` fields, and fields with a `[default: 'raw_sql']`
attribute, are not sent to the database when the value being sent is the default
for the V struct field (e.g., 0 int, or an empty string).  This allows the
database to insert default values for auto-increment fields and where you have
specified a default.

### Select

You can select rows from the database by passing the struct as the table, and
use V syntax and functions for expressions. Selecting returns an array of the
results.

```v ignore
result := sql db {
    select from Foo where id == 1
}!

foo := result.first()
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


### Update

You can update fields in a row using V syntax and functions. Again, pass the struct
as the table.

```v ignore
sql db {
    update Foo set updated_at = time.now() where name == 'abc' && updated_at is none
}!
```

Note that `is none` and `!is none` can be used to select for NULL fields.

### Delete

You can delete rows using V syntax and functions. Again, pass the struct
as the table.

```v ignore
sql db {
    delete from Foo where id > 10
}!
```

### time.Time Fields

It's definitely useful to cast a field as `time.Time` so you can use V's built-in time functions;
however, this is handled a bit differently than expected in the ORM. `time.Time` fields are 
created as integer columns in the database. Because of this, the usual time functions 
(`current_timestamp`, `NOW()`, etc) in SQL do not work as defaults.

## Example

```v ignore
import db.pg

struct Member {
	id         string @[default: 'gen_random_uuid()'; primary; sql_type: 'uuid']
	name       string
	created_at string @[default: 'CURRENT_TIMESTAMP'; sql_type: 'TIMESTAMP']
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
