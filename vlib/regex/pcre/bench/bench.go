package main

import (
	"fmt"
	"regexp"
	"strings"
	"time"
)

const iterations = 10000
const warmup = 100

var shortText = "The quick brown fox jumps over the lazy dog 12345"
var mediumText = "The quick brown fox jumps over the lazy dog 12345. " +
	"Pack my box with five dozen liquor jugs. " +
	"How vexingly quick daft zebras jump! " +
	"The five boxing wizards jump quickly. " +
	"Sphinx of black quartz, judge my vow. " +
	"Two driven jocks help fax my big quiz."
var longText = strings.Repeat(mediumText, 20)
var emailText = "Contact us at user@example.com or admin@test.org for info. " +
	"Also try support@company.co.uk and test.user+tag@domain.com please."

type BenchResult struct {
	Name    string
	NsPerOp int64
	OpsSec  float64
}

func bench(name string, f func() int) BenchResult {
	for i := 0; i < warmup; i++ {
		f()
	}
	start := time.Now()
	for i := 0; i < iterations; i++ {
		f()
	}
	elapsed := time.Since(start)
	nsPerOp := elapsed.Nanoseconds() / int64(iterations)
	opsSec := float64(0)
	if nsPerOp > 0 {
		opsSec = 1e9 / float64(nsPerOp)
	}
	return BenchResult{Name: name, NsPerOp: nsPerOp, OpsSec: opsSec}
}

func main() {
	results := []BenchResult{}

	// 1. Literal match
	re1 := regexp.MustCompile(`quick`)
	results = append(results, bench("1. Literal match (short)", func() int {
		if re1.FindStringIndex(shortText) != nil {
			return 1
		}
		return 0
	}))

	// 2. Char class
	re2 := regexp.MustCompile(`[a-zA-Z]+`)
	results = append(results, bench("2. Char class [a-zA-Z]+", func() int {
		if re2.FindStringIndex(mediumText) != nil {
			return 1
		}
		return 0
	}))

	// 3. Alternation
	re3 := regexp.MustCompile(`fox|dog|cat|bird`)
	results = append(results, bench("3. Alternation (4 words)", func() int {
		if re3.FindStringIndex(shortText) != nil {
			return 1
		}
		return 0
	}))

	// 4. Find all digits
	re4 := regexp.MustCompile(`\d+`)
	results = append(results, bench("4. Find all digits", func() int {
		return len(re4.FindAllString(mediumText, -1))
	}))

	// 5. Groups
	re5 := regexp.MustCompile(`(\w+)\s+(\w+)`)
	results = append(results, bench("5. Groups (\\w+)\\s+(\\w+)", func() int {
		if re5.FindStringSubmatchIndex(mediumText) != nil {
			return 1
		}
		return 0
	}))

	// 6. Email
	re6 := regexp.MustCompile(`[\w.]+@[\w]+\.[\w]+`)
	results = append(results, bench("6. Email pattern", func() int {
		return len(re6.FindAllString(emailText, -1))
	}))

	// 7. Long text
	re7 := regexp.MustCompile(`quickly`)
	results = append(results, bench("7. Long text literal scan", func() int {
		return len(re7.FindAllString(longText, -1))
	}))

	// 8. Replace all
	re8 := regexp.MustCompile(`\d+`)
	results = append(results, bench("8. Replace all digits", func() int {
		return len(re8.ReplaceAllString(mediumText, "NUM"))
	}))

	// 9. Dot-star
	re9 := regexp.MustCompile(`.*fox.*dog`)
	results = append(results, bench("9. Dot-star greedy", func() int {
		if re9.MatchString(shortText) {
			return 1
		}
		return 0
	}))

	// 10. Compile speed
	results = append(results, bench("10. Compile simple", func() int {
		_, err := regexp.Compile(`\w+@\w+\.\w+`)
		if err != nil {
			return 0
		}
		return 1
	}))

	// 11. Lookahead (Go doesn't support — skip)
	results = append(results, BenchResult{Name: "11. Lookahead", NsPerOp: -1, OpsSec: 0})

	// 12. Backreference (Go doesn't support — skip)
	results = append(results, BenchResult{Name: "12. Backref", NsPerOp: -1, OpsSec: 0})

	// Print
	fmt.Println(strings.Repeat("=", 70))
	fmt.Println("                 GO REGEX BENCHMARK (regexp)")
	fmt.Println(strings.Repeat("=", 70))
	fmt.Printf("%-35s %12s %14s\n", "Benchmark", "ns/op", "ops/sec")
	fmt.Println(strings.Repeat("-", 70))
	for _, r := range results {
		if r.NsPerOp < 0 {
			fmt.Printf("%-35s %12s %14s\n", r.Name, "N/A", "unsupported")
		} else {
			fmt.Printf("%-35s %12d %14.0f\n", r.Name, r.NsPerOp, r.OpsSec)
		}
	}
	fmt.Println(strings.Repeat("=", 70))
}
