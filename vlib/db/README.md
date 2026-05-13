## Description

`db` is a namespace that contains several useful modules 
for operating with databases (SQLite, MySQL, MSQL, etc.)

## Cross-driver consistency helpers

`db.pg` and `db.mysql` accept both `user` and `username` in their `Config` structs.

`db.pg`, `db.mysql`, and `db.sqlite` rows expose `row.val(index)` and `row.values()`
for direct string access. In `db.pg`, SQL `NULL` remains available through
`row.val_opt(index)`.

`db.pg`, `db.mysql`, and `db.sqlite` also expose `exec_param2(...)` as a convenience
wrapper around their parameterized query helpers.
