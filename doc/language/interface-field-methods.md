# Methods on interface fields after smartcasts

After an interface value is narrowed to a concrete struct with `is` or `match`,
its interface fields keep normal method dispatch. For example, if the concrete
struct has an interface field `inner`, calling `value.inner.name()` inside the
matching branch dispatches through `inner` on that concrete object.

An imported interface keeps its identity when the importing module declares a struct
with the same short name. For example, a local `struct Reader` does not change a
`streamreader.Reader` interface value's type tests, boxed data or method dispatch.

Assigning an optional concrete value or pointer to an optional interface field preserves
`none`. A present value keeps its methods and interface fields. A concrete pointer retains
its backing object, so mutable method calls update that same object.
