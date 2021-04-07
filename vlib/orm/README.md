# ORM

## Attributes

### Fields

- `[primary]` set the field as the primary key
- `[nonull]` field will be `NOT NULL` in table creation
- `[skip]` field will be skipped
- `[sql: type]` sets the type which is used in sql (special type `serial`)

## Usage

```v
struct Foo {
    id   int    [primary; sql: serial]
    name string [nonull]
}
```

### Create

```v
sql db {
    create table Foo
}
```

### Insert

```v
var := Foo{
    name: 'abc'
}

sql db {
    insert var into Foo
}
```

### Update

```v
sql db {
    update Foo set name = 'cde' where name == 'abc'
}
```

### Delete
```v
sql db {
    delete from Foo where id > 10
}
```

### Select
```v
result := sql db {
    select from Foo where id == 1
}
```
```v
result := sql db {
    select from Foo where id > 1 limit 5
}
```
```v
result := sql db {
    select from Foo where id > 1 order by id
}
```