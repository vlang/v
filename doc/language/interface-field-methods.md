# Methods on interface fields after smartcasts

After an interface value is narrowed to a concrete struct with `is` or `match`,
its interface fields keep normal method dispatch. For example, if the concrete
struct has an interface field `inner`, calling `value.inner.name()` inside the
matching branch dispatches through `inner` on that concrete object.

An imported interface keeps its identity when the importing module declares a struct
with the same short name. For example, a local `struct Reader` does not change a
`streamreader.Reader` interface value's type tests, boxed data or method dispatch.
