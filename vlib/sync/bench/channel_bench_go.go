package main

import "fmt"
import "log"
import "os"
import "time"
import "strconv"

func assert_eq(a, b int64) {
	if a != b {
		log.Fatalf("assertion failed\nleft: %d, right: %d\n", a, b)
	}
}

func do_rec(ch chan int32, resch chan int64, n int32) {
	var sum int64
	var i int32
	for i = 0; i < n; i++ {
		sum += int64(<- ch)
	}
	fmt.Println(sum)
	resch <- sum
}

func do_send(ch chan int32, start, end int32) {
	for i := start; i < end; i++ {
		ch <- i
	}
}

func main() {
	if len(os.Args) != 5 {
		log.Fatalf("usage:\n\t%s <nsend> <nrec> <buflen> <nobj>\n", os.Args[0])
	}
	nsend, _ := strconv.Atoi(os.Args[1])
	nrec, _ := strconv.Atoi(os.Args[2])
	buflen, _ := strconv.Atoi(os.Args[3])
	nobj, _ := strconv.Atoi(os.Args[4])
	stopwatch := time.Now()
	ch := make(chan int32, buflen)
	resch := make(chan int64, 0)
	no := nobj
	for i := 0; i < nrec; i++ {
		n := no / (nrec - i)
		go do_rec(ch, resch, int32(n))
		no -= n
	}
	assert_eq(int64(no), 0)	
	no = nobj
	for i := 0; i < nsend; i++ {
		n := no / (nsend - i)
		end := no
		no -= n
		go do_send(ch, int32(no), int32(end))
	}
	assert_eq(int64(no), 0)	
	var sum int64
	for i := 0; i < nrec; i++ {
		sum += <-resch
	}
	elapsed := time.Now().Sub(stopwatch)
	rate := float64(nobj)/float64(elapsed.Nanoseconds())*1000.0
	duration := 1.0e-09 * float64(elapsed.Nanoseconds())
	fmt.Printf("%d objects in %g s (%.2f objs/µs)\n", nobj, duration, rate)
	expected_sum := int64(nobj)*int64(nobj-1)/2
	fmt.Printf("got: %d, expected: %d\n", sum, expected_sum)
	assert_eq(sum, expected_sum)
}
