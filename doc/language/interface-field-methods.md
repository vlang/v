# Methods on interface fields after smartcasts

After an interface value is narrowed to a concrete struct with `is` or `match`,
its interface fields keep normal method dispatch. For example, if the concrete
struct has an interface field `inner`, calling `value.inner.name()` inside the
matching branch dispatches through `inner` on that concrete object.
