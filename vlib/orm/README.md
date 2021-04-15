# ORM

## Attributes

### Structs

- `[tablename: 'name']` sets a custom table name

### Fields

- `[primary]` sets the field as the primary key
- `[unique]` sets the field as unique
- `[unique: 'foo']` adds the field to a unique group
- `[skip]` field will be skipped
- `[sql: type]` sets the type which is used in sql (special type `serial`)
- `[sql: 'name']` sets a custom column name for the field

## Usage

```v ignore
struct Foo {
    id   int    [primary; sql: serial]
    name string [nonull]
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
    name: 'abc'
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