/*
This example demonstrates thread safety using channels in V.

### Functions:
- `producer(ch chan int)`: This function simulates a producer that sends integers from 1 to 99 to
the channel `ch`. It prints each produced item.
- `consumer(ch chan int)`: This function simulates a consumer that receives integers from the
channel `ch`.

### Thread Safety:
- The use of channels ensures thread safety by providing a synchronized way to communicate between
the producer and consumer threads.
- Channels in V are designed to handle concurrent access, preventing race conditions and ensuring
that data is safely passed between threads.
- The `select` statement in the consumer function allows it to handle timeouts gracefully,
ensuring that the program does not hang if the producer is not ready.
*/
import time

fn producer(ch chan int) {
	for i in 1 .. 100 {
		ch <- i
		println('Produced: ${i}')
	}
}

fn consumer(ch chan int) {
	for {
		select {
			item := <-ch {
				println('Consumed: ${item}')
			}
			500 * time.millisecond {
				println('Timeout: No producers were ready within 0.5s')
				break
			}
		}
	}
}

fn main() {
	ch := chan int{cap: 10}

	producer_thread := spawn producer(ch)
	consumer_thread := spawn consumer(ch)

	producer_thread.wait()
	consumer_thread.wait()
}
