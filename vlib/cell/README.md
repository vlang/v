# cell

`RefCell[T]` checks callback access at runtime for single-threaded code.
Use `new_ref_cell(value)` to create a cell, `with(callback)` to read its value,
and `with_mut(callback)` to replace it with the callback's return value.
Shared callbacks may nest. An exclusive callback excludes both shared and exclusive access.
The `try_with` and `try_borrow` methods return `none` on a conflicting exclusive borrow;
`try_with_mut` returns `false` on any conflict. The other methods panic on conflicts.

Borrow counts are released when a callback returns. `try_borrow` does not keep a borrow active.
Values follow ordinary V assignment semantics, including references contained in `T`;
the cell does not provide deep copies or track aliases retained outside callbacks.
Do not mutate such aliases during shared access. Cells do not synchronize access across threads.
