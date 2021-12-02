# onecontext

A library to merge existing V contexts.

## Overview

Have you ever faced the situation where you have to merge multiple existing contexts?
If not, then you might, eventually.

For example, we can face the situation where we are building an application
using a library that gives us a global context.
This context expires once the application is stopped.

Meanwhile, we are exposing a service like this:

```v ignore
fn (f Foo) get(ctx context.Context, bar Bar) ?Baz {
	. . .
}
```

Here, we receive another context provided by the service.

Then, in the `get` implementation, we want for example to query a database and
we must provide a context for that.

Ideally, we would like to provide a merged context that would expire either:

- When the application is stopped
- Or when the received service context expires

This is exactly the purpose of this library.

In our case, we can now merge the two contexts in a single one like this:

```v ignore
ctx, cancel := onecontext.merge(ctx1, ctx2)
```

This returns a merged context that we can now propagate
