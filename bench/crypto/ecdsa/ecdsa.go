package main

import (
    "crypto/ecdsa"
    "crypto/elliptic"
    "crypto/rand"
    "fmt"
    "time"
)

func main() {
    iterations := 1000

    fmt.Println("Benchmarking key generation...")
    var totalGenTime int64
    for i := 0; i < iterations; i++ {
        start := time.Now()
        _, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
        if err != nil {
            panic(err)
        }
        totalGenTime += time.Since(start).Microseconds()
    }
    avgGenTime := totalGenTime / int64(iterations)
    fmt.Printf("Average key generation time: %d µs\n", avgGenTime)

    privKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
    if err != nil {
        panic(err)
    }
    message := []byte("Benchmark message")

    fmt.Println("Benchmarking signing...")
    var totalSignTime int64
    for i := 0; i < iterations; i++ {
        start := time.Now()
        _, _, err := ecdsa.Sign(rand.Reader, privKey, message)
        if err != nil {
            panic(err)
        }
        totalSignTime += time.Since(start).Microseconds()
    }
    avgSignTime := totalSignTime / int64(iterations)
    fmt.Printf("Average sign time: %d µs\n", avgSignTime)

    r, s, err := ecdsa.Sign(rand.Reader, privKey, message)
    if err != nil {
        panic(err)
    }

    pubKey := &privKey.PublicKey

    fmt.Println("Benchmarking verification...")
    var totalVerifyTime int64
    for i := 0; i < iterations; i++ {
        start := time.Now()
        ecdsa.Verify(pubKey, message, r, s)
        totalVerifyTime += time.Since(start).Microseconds()
    }
    avgVerifyTime := totalVerifyTime / int64(iterations)
    fmt.Printf("Average verify time: %d µs\n", avgVerifyTime)
}

