## Description

`trace` records manual spans using the monotonic clock. Create a tracer with `new_tracer()`,
open a span with `tracer.span(name)`, and call `span.end()` when the operation finishes.
The same span instance ignores repeated `end()` calls; copies have independent completion state
and should not both be ended. A span records the thread where it started, even if ended elsewhere.

`report()` returns per-name counts and total, average, and maximum durations in nanoseconds,
sorted by total duration. `to_chrome_trace()` exports completed spans as Chrome trace-event JSON
for Perfetto or Chrome tracing, with microsecond timestamps and durations.
`event_count()` counts completed spans. `reset()` clears recorded events and aggregates; spans
that remain open can still record when ended.

The tracer synchronizes its shared aggregates and event log. A span instance itself must be
ended by one caller at a time. The log retains every completed event until reset and does not
perform sampling or allocation tracking.
