# net.http integration tests

These tests use `net.http` request and response values in memory. They do not
open a listener, bind a port, or call an external service.

The coverage target is the `x.async` boundary around HTTP-shaped work:

- bounded pool submission;
- explicit queue-full backpressure;
- draining accepted request handlers through `Pool.close()`;
- preserving response data produced by accepted jobs.
