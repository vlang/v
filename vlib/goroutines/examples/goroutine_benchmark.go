// Goroutine benchmark in Go – equivalent to goroutine_benchmark.v.
//
// Tests:
// 1. Goroutine creation + completion (fan-out/fan-in via unbuffered channel)
// 2. Channel ping-pong between two goroutines
// 3. Many goroutines contending on a single channel
//
// Run: go run goroutine_benchmark.go
package main

import (
	"fmt"
	"time"
)

// ---------------------------------------------------------------------------
// Benchmark 1 – fan-out / fan-in (unbuffered channel)
// ---------------------------------------------------------------------------

func benchFanOutFanIn(n int) {
	c := make(chan int) // unbuffered, matching V benchmark

	start := time.Now()

	for i := 0; i < n; i++ {
		go func() {
			c <- 1
		}()
	}

	for i := 0; i < n; i++ {
		<-c
	}

	elapsed := time.Since(start)
	fmt.Printf("fan-out/fan-in  %d goroutines: %d us  (%d ns/goroutine)\n",
		n, elapsed.Microseconds(), elapsed.Nanoseconds()/int64(n))
}

// ---------------------------------------------------------------------------
// Benchmark 2 – channel ping-pong
// ---------------------------------------------------------------------------

func benchPingPong(n int) {
	c1 := make(chan int, 1)
	c2 := make(chan int, 1)

	go func() {
		for i := 0; i < n; i++ {
			val := <-c1
			c2 <- val + 1
		}
	}()

	start := time.Now()

	val := 0
	for i := 0; i < n; i++ {
		c1 <- val
		val = <-c2
	}

	elapsed := time.Since(start)
	fmt.Printf("ping-pong       %d round-trips: %d us  (%d ns/round-trip)\n",
		n, elapsed.Microseconds(), elapsed.Nanoseconds()/int64(n))
}

// ---------------------------------------------------------------------------
// Benchmark 3 – contended channel (many producers, one consumer)
// ---------------------------------------------------------------------------

func benchContendedChannel(numProducers int, msgsPerProducer int) {
	total := numProducers * msgsPerProducer
	c := make(chan int, 64)

	start := time.Now()

	for p := 0; p < numProducers; p++ {
		go func() {
			for i := 0; i < msgsPerProducer; i++ {
				c <- i
			}
		}()
	}

	for i := 0; i < total; i++ {
		<-c
	}

	elapsed := time.Since(start)
	fmt.Printf("contended chan  %d producers x %d msgs: %d us  (%d ns/msg)\n",
		numProducers, msgsPerProducer,
		elapsed.Microseconds(), elapsed.Nanoseconds()/int64(total))
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

func main() {
	fmt.Println("=== Go Goroutine Benchmark ===")
	fmt.Println()

	benchFanOutFanIn(10)
	benchFanOutFanIn(50)
	benchFanOutFanIn(100)
	benchFanOutFanIn(500)

	fmt.Println()
	benchPingPong(1000)
	benchPingPong(10000)
	benchPingPong(100000)

	fmt.Println()
	benchContendedChannel(4, 1000)
	benchContendedChannel(10, 1000)

	fmt.Println()
}
