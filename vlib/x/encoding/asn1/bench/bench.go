package main

import (
	"encoding/asn1"
	"fmt"
	"time"
)

type Example struct {
	Greeting string `asn1:"utf8"`
	Answer   int
	Tipe     asn1.ObjectIdentifier `asn1:"explicit,tag:1"`
}

func main() {
	iterations := 1000
	expected_output := []byte{0x30, 18, 12, 5, 72, 101, 108, 108, 111, 2, 1, 42, 0xA1,
		6, 6, 4, 43, 6, 1, 3}

	oid := []int{1, 3, 6, 1, 3}

	ex := Example{
		Greeting: "Hello",
		Answer:   42,
		Tipe:     asn1.ObjectIdentifier(oid),
	}
	fmt.Println("Benchmarking golang Marshal...")
	var totalMarshalTime int64
	for i := 0; i < iterations; i++ {
		start := time.Now()
		_, err := asn1.Marshal(ex)
		if err != nil {
			panic(err)
		}
		totalMarshalTime += time.Since(start).Microseconds()
	}
	avgMarshalTime := totalMarshalTime / int64(iterations)
	fmt.Printf("Average Marshal time: %d µs\n", avgMarshalTime)

	fmt.Println("Benchmarking Unmarshal...")
	var totalUnmarshalTime int64
	var xx Example
	for i := 0; i < iterations; i++ {
		start := time.Now()
		_, err := asn1.Unmarshal(expected_output, &xx)
		if err != nil {
			panic(err)
		}
		totalUnmarshalTime += time.Since(start).Microseconds()
	}
	avgUnmarshalTime := totalUnmarshalTime / int64(iterations)
	fmt.Printf("Average Unmarshal time: %d µs\n", avgUnmarshalTime)
}
