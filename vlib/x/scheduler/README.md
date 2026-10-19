# In-process scheduler

`Scheduler` runs interval and daily jobs sequentially in one loop thread.
Register jobs before calling `start`, and call `stop` to wait for the loop to finish.

The loop ticks once per second. Positive intervals passed to `every` are rounded up to whole
seconds, so a 1500 millisecond interval runs at most once every two seconds. Interval jobs also run
on the first tick after startup. `run_pending` accepts a Unix timestamp in seconds for deterministic
scheduling without starting the loop.

Daily jobs run when a tick falls inside the requested minute; missed times are not replayed.
