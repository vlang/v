Benchmark of `x.encoding.asn1` module
--------------------------

This is a benchmark of `x.encoding.asn1` module compared with go version (on free gitpod workspace)

Regular benchmark produces this result:
```bash
(dev) $ v run bench/bench.v
Benchmarking ASN.1 encode...
Average example encode time: 13 µs
Benchmarking ASN.1 decode (with asn.decode)...
Average (asn1.decode) decode time: 3 µs
Benchmarking ASN.1 decode with Example.decode)...
Average (Example.decode) decode time: 2 µs
```

Build with `-prod` flag and rerun the bench
```bash
$ v -prod benchk/bench.v
gitpod /workspace/asn1 (dev) $ ./bench/bench
Benchmarking ASN.1 encode...
Average example encode time: 3 µs
Benchmarking ASN.1 decode (with asn.decode)...
Average (asn1.decode) decode time: 1 µs
Benchmarking ASN.1 decode with Example.decode)...
Average (Example.decode) decode time: 1 µs
```

The go version produces following result:
```bash
$ go run bench/bench.go
Benchmarking golang Marshal...
Average Marshal time: 1 µs
Benchmarking Unmarshal...
Average Unmarshal time: 0 µs
```