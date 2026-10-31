# Channels

> The typed default is in the parent skill. This reference covers: closing,
> buffered versus unbuffered, `select`, and the deadlocks.

## Creating one

```v ignore
// Unbuffered: every send blocks until a receiver takes it.
mut ch := chan int{}

// Buffered: a sender gets ahead by up to the capacity.
mut jobs := chan Job{cap: 16}
```

The capacity is part of the type, so a buffered and an unbuffered channel of the
same element type are different types. Pick the buffer when a producer should not
block on a slow consumer.

## Sending and receiving

```v ignore
ch <- 42              // send, blocks when full or unbuffered
v := <-ch             // receive, blocks when empty
v := <-ch or { -1 }   // receive with a default, non-blocking
```

`<-ch or { default }` is the way to poll without blocking. Use it when the channel
may simply be empty and that is not an error.

## Closing

Close a channel when the producer is finished with it. A receiver can tell a
closed channel from an empty one only by draining it:

```v ignore
mut results := chan Result{cap: 16}

spawn fn () {
	for job in jobs {
		results <- run(job)
	}
	close(results)
}

for r in results {
	println(r)     // ends when the channel is closed and drained
}
```

Rules that prevent the usual bugs:

- **Close from the producer, once.** Closing twice panics.
- **Never send on a closed channel.** It panics.
- **A `for` loop over a channel ends when it is closed and drained.** That is the
  idiomatic consumer.
- **`close(errs ...IError)`** carries a reason, so a consumer can see why the
  producer stopped rather than inferring it.

## Worker pool

The shape for a bounded amount of parallel work: a queue, a fixed set of consumers,
and a wait.

```v ignore
import sync

fn process_all(jobs []Job) []Result {
	mut queue := chan Job{len: jobs.len}
	mut results := []Result{cap: jobs.len}
	mut wg := sync.new_waitgroup()

	for job in jobs {
		queue <- job
	}
	close(queue)

	mut mu := sync.new_mutex()
	for _ in 0 .. runtime.nr_jobs() {
		wg.go(fn () {
			for job in queue {
				r := run(job)
				lock mu {
					results << r
				}
			}
		})
	}
	wg.wait()
	return results
}
```

Queue is filled and closed **before** the consumers start, which is what keeps a
consumer from blocking on a channel nobody will close. The lock is held only while
appending, never across `run`.

## select

`select` waits on several channels and takes whichever is ready first.

```v ignore
mut chans := [a, b, c]
mut dirs := [sync.Direction.recv, sync.Direction.recv, sync.Direction.recv]
mut objs := []voidptr{&va, &vb, &vc}

ready := sync.channel_select(mut chans, dirs, mut objs, timeout_ms)
if ready >= 0 {
    println('chose channel ${ready}')
}
```

The `timeout` is in milliseconds; pass a negative value to wait indefinitely. This
is how you implement a timeout, a cancellation channel, or a fair merge of several
producers — all of which otherwise need a shared lock and a flag.

## Deadlocks

The two that account for most of them:

**Nobody drains the channel.** The producer blocks on send forever, and the
consumer is waiting for the producer.

```v ignore
// Deadlock: the buffer is smaller than the job list and no consumer runs yet.
mut ch := chan Job{cap: 4}
for job in jobs {
    ch <- job       // blocks on the fifth
}
spawn consumer(ch)  // too late
```

Fill and close the queue before starting the consumers, or make the buffer as large
as the producer's output.

**Two locks in two orders.** Thread A holds `mu1` and wants `mu2`; thread B holds
`mu2` and wants `mu1`. Nothing is blocked on I/O, so no timeout saves you.

The fix is a lock order, applied everywhere:

```v ignore
// Always mu1 before mu2, in every function that needs both.
lock mu1 {
    lock mu2 {
        ...
    }
}
```

**Holding a lock across a channel operation.** If the consumer of that channel
needs the same lock, it can never take it. Copy what you need, release, then send.

## Testing

Channels make tests timing-dependent. To keep a test reliable:

- use a `sync.Once` or a buffered channel so setup happens once;
- give every channel a capacity, so a send never blocks on scheduling;
- assert on the drained result rather than on an intermediate state.

```bash
v -race test dir/
```

A race that only appears under the detector is a real race that happened not to
fire. See the parent skill.