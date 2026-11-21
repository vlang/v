# Channels

> The typed default is in the parent skill. This reference covers: closing,
> buffered versus unbuffered, `select`, and the deadlocks.

## Creating one

```v ignore
// Unbuffered: every send blocks until a receiver takes it.
ch := chan int{}

// Buffered: a sender gets ahead by up to the capacity.
jobs := chan Job{cap: 16}
```

The capacity is not part of the type: both are passed wherever a `chan int` or a
`chan Job` is expected. Pick a buffer when a producer should not block on a slow
consumer. Use `cap`, not `len`: `chan int{len: 3}` compiles but leaves the channel
unbuffered.

## Sending and receiving

```v ignore
ch <- 42                     // send; blocks while the buffer is full
v := <-ch                    // receive; blocks while the channel is empty
v := <-ch or { -1 }          // blocks too; `or` runs once ch is closed and drained
ch <- 42 or { println('closed') } // a send to a closed channel goes to `or`
```

`or` on a receive is not a poll: it waits for a value, and runs only when the
channel is closed and has nothing left. To check without blocking, use
`try_pop`, or a `select` with an `else` branch:

```v
fn main() {
	ch := chan int{cap: 1}
	mut v := 0
	if ch.try_pop(mut v) == .not_ready {
		println('nothing yet')
	}
}
```

## Closing

Close a channel, with the method `ch.close()`, when the producer is finished with
it. A receiver sees the close once it has drained what was sent before:

```v
fn produce(results chan int) {
	for i in 0 .. 3 {
		results <- i
	}
	results.close()
}

fn main() {
	results := chan int{cap: 4}
	spawn produce(results)
	for {
		r := <-results or { break } // ends when the channel is closed and drained
		println(r)
	}
}
```

Rules that prevent the usual bugs:

- **Close from the producer.** A second `close()` does nothing, but a consumer
  closing a channel its producer still writes to makes that producer panic.
- **Never send on a closed channel without `or`.** A plain send panics with
  `push on closed channel`.
- **`for x in ch` does not compile.** Loop on `<-ch or { break }`, as above.
- **`ch.close(err)` carries a reason**, which the receiver gets as `err` in its
  `or` block, so a consumer can see why the producer stopped.

## Worker pool

The shape for a bounded amount of parallel work: a queue, a fixed set of
consumers, and a wait. Start the consumers before filling the queue, or give the
queue room for every job.

```v
import runtime

fn consume(queue chan int, results chan int) {
	for {
		job := <-queue or { break }
		results <- job * job
	}
}

fn process_all(jobs []int) []int {
	queue := chan int{cap: jobs.len}
	results := chan int{cap: jobs.len}
	mut workers := []thread{}
	for _ in 0 .. runtime.nr_jobs() {
		workers << spawn consume(queue, results)
	}
	for job in jobs {
		queue <- job
	}
	queue.close()
	workers.wait()
	mut out := []int{cap: jobs.len}
	for _ in 0 .. jobs.len {
		out << <-results
	}
	return out
}

fn main() {
	assert process_all([1, 2, 3]).len == 3
}
```

`results` has room for every result, so no worker blocks on it before the
collector runs. The results arrive in completion order, not input order; use
`parallel.amap` when the order matters.

## select

`select` waits on several channel operations and takes whichever is ready first.
A branch that is a duration is a timeout; an `else` branch makes it non-blocking.

```v
import time

fn main() {
	a := chan int{cap: 1}
	b := chan string{cap: 1}
	b <- 'ready'
	select {
		n := <-a {
			println('a: ${n}')
		}
		s := <-b {
			println('b: ${s}')
		}
		500 * time.millisecond {
			println('timed out')
		}
	}
}
```

This is how you implement a timeout, a cancellation channel, or a merge of
several producers — all of which otherwise need a shared lock and a flag.

## Deadlocks

The two that account for most of them:

**Nobody drains the channel.** The producer blocks on send forever, and the
consumer is waiting for the producer.

```v ignore
// Deadlock: the buffer is smaller than the job list and no consumer runs yet.
ch := chan Job{cap: 4}
for job in jobs {
	ch <- job // blocks on the fifth
}
spawn consumer(ch) // too late
```

Start the consumers before sending, or make the buffer as large as everything
that will be sent before anyone receives.

**Two locks in two orders.** Thread A holds the first and wants the second;
thread B holds the second and wants the first. Nothing is blocked on I/O, so no
timeout saves you. The fix is a lock order, applied everywhere:

```v ignore
// Always `a` before `b`, in every function that needs both.
lock a {
	lock b {
		// ...
	}
}
```

**Holding a lock across a channel operation.** If the consumer of that channel
needs the same lock, it can never take it. Copy what you need, release, then send.

## Testing

Channels make tests timing-dependent. To keep a test reliable:

- give a channel room for everything a test sends before it receives, or start
  the receiver first — a full buffer blocks a send like an unbuffered channel;
- bound every wait with a `select` timeout, so a bug fails the test instead of
  hanging it;
- assert on the drained result rather than on an intermediate state.

```bash
v -race test dir/
```

A race that only appears under the detector is a real race that happened not to
fire. See the parent skill.
