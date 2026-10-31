module fnaliascontext

pub struct Context {
pub mut:
	value int
}

pub type Handler[T] = fn (mut T) bool

pub type Mapper[T] = fn (T) T

// apply calls a type-erased handler with the caller's context.
pub fn apply[T](mut ctx T, raw voidptr) bool {
	callback := Handler[T](raw)
	return callback(mut ctx)
}

// map calls a type-erased callback that accepts and returns the caller's type.
pub fn map[T](ctx T, raw voidptr) T {
	callback := Mapper[T](raw)
	return callback(ctx)
}
