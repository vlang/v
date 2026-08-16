# V3 ORM migrations

`v3.migrations` provides ordered, reversible ORM migrations for the V3 compiler. It records
applied versions in `schema_migrations`, uses transactions when the database supports transactional
DDL, and supports migrate, rollback, redo, reset, target-version, and status workflows.

Mutating workflows hold a database-level lock from before the applied-version snapshot through all
callbacks and history updates. PostgreSQL uses an advisory lock, MySQL uses a named lock, and
SQLite uses an immediate transaction so concurrent runners cannot apply the same migration.

```v ignore
import db.sqlite
import v3.migrations

fn create_users(mut ctx migrations.Context) ! {
    ctx.create_table(migrations.Table{
        name: 'users'
        columns: [
            migrations.Column{
                name: 'email'
                kind: .varchar
                nullable: false
            },
        ]
    })!
}

fn drop_users(mut ctx migrations.Context) ! {
    ctx.drop_table('users')!
}

mut db := sqlite.connect('app.db')!
mut migrator := migrations.new(mut db, [
    migrations.Migration{
        version: 20260816143000
        name: 'create_users'
        up: create_users
        down: drop_users
    },
], migrations.Config{
    dialect: .sqlite
})!

migrator.migrate()!
```

Migration callbacks receive a context that implements `orm.Connection`. Existing ORM DDL and DML
can therefore be mixed with migration helpers:

```v ignore
fn create_users(mut ctx migrations.Context) ! {
    sql ctx {
        create table User
    }!
}
```

The schema helpers include table and column creation/removal/renaming, indexes, inline or altered
foreign keys, and trusted raw SQL via `ctx.execute()`. SQLite cannot directly change a column or
add/remove a foreign key on an existing table; those helpers return an error so the migration can
explicitly rebuild the table. SQLite `add_column` also rejects primary-key, unique, and
auto-increment columns, non-nullable columns without a non-NULL default, and defaults that are not
constant. SQLite index tables and foreign-key targets must be unqualified. PostgreSQL
`change_column` supports type-related fields only and rejects explicitly supplied constraint
options, including false or empty values, before executing SQL; use `ctx.execute()` for explicit
constraint DDL. MySQL `change_column` requires all optional constraint fields because `MODIFY
COLUMN` replaces the complete definition, and MySQL auto-increment columns must be primary keys or
unique. Column-level identifiers must be unqualified. PostgreSQL and SQLite table rename targets
must also be unqualified; MySQL keeps support for qualified table targets. MySQL migrations default
to non-transactional execution because MySQL DDL implicitly commits. The migrator accepts
`orm.TransactionalConnection` implementations, and `Config.transaction_mode` can override whether
per-migration transaction methods are used for DDL. SQLite's immediate lock transaction still
covers each mutating workflow.
