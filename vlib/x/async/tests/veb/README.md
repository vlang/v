# veb integration tests

These tests use `veb.Context` in memory. They do not start a `veb` server, bind
a port, or depend on HTTP client/server timing.

The coverage target is narrow: `x.async` can bound and propagate errors for a
synthetic veb handler step without becoming part of `veb`'s runtime or server
lifecycle.
