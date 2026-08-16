# V3 ORM migrations

`v3.migrations` provides ordered, reversible ORM migrations for the V3 compiler. It records
applied versions in `schema_migrations`, uses transactions when the database supports transactional
DDL, and supports migrate, rollback, redo, reset, target-version, and status workflows.

Mutating workflows hold a database-level lock from before the applied-version snapshot through all
callbacks and history updates. PostgreSQL uses an advisory lock, MySQL uses a named lock, and
SQLite uses an immediate transaction so concurrent runners cannot apply the same migration. MySQL
lock names use a qualified history table's database, or otherwise the current database, so
independent databases on one server do not contend, and follow the server's
`lower_case_table_names` mode. PostgreSQL lock keys use the effective history schema and the full
signed 64-bit advisory-lock space.

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
auto-increment columns, non-nullable columns without a non-NULL default, and nonconstant defaults
even when prohibited expressions have SQL comments. Parenthesized literal defaults remain allowed.
SQLite index tables and foreign-key targets must be unqualified; index removal derives the index
schema from a qualified table. PostgreSQL `change_column` supports type-related fields only and
rejects explicitly supplied constraint options, including false or empty values, before executing
SQL; use `ctx.execute()` for explicit constraint DDL. PostgreSQL serial columns reject
explicit defaults, and index removal derives the index schema from a qualified table; PostgreSQL
index names are unqualified when adding them.
SQLite non-integer primary keys are explicitly non-nullable. Decimal scale requires a positive
precision. MySQL `change_column` requires nullable, default, and auto-increment attributes; omitted
key options, including those on auto-increment columns, are preserved, additions use `true`, and
removals must use `remove_index()` or raw SQL.
MySQL auto-increment columns must be primary keys or unique, MySQL index names must be unqualified
when adding or removing them, and tables cannot contain more than one auto-increment column. MySQL
foreign keys reject `SET DEFAULT`. Column-level identifiers must be unqualified. Generated
PostgreSQL and MySQL index and foreign-key names are shortened deterministically to their dialect
limits; ambiguous index component boundaries and generated PostgreSQL and MySQL foreign-key
identities receive deterministic hash suffixes, and overlong explicit names are rejected.
Caller-supplied table, column, and history-table name components are also checked against those
dialect limits, and qualified table, history, or index names may contain at most two components.
SQLite and MySQL reject case-insensitive duplicate table columns.
PostgreSQL and SQLite table rename targets must also be unqualified; MySQL keeps support for
qualified table targets. MySQL migrations default to non-transactional execution because MySQL DDL
implicitly commits. MySQL history strings use hex literals, while PostgreSQL uses explicit escape
strings with doubled backslashes, avoiding session-mode-sensitive escaping.
The migrator accepts `orm.TransactionalConnection` implementations, and `Config.transaction_mode`
can override whether per-migration transaction methods are used for DDL. SQLite's immediate lock
transaction still covers each mutating workflow.
