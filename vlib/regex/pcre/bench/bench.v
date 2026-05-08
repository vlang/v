import regex
import regex.pcre
import time

const iterations = 10000
const warmup = 100

struct BenchResult {
	name      string
	engine    string
	ns_per_op i64
	ops_sec   f64
}

fn fmt_col(s string, width int) string {
	if s.len >= width {
		return s[..width]
	}
	return s + ' '.repeat(width - s.len)
}

// format_ops formats ops/sec with comma separators for readability.
fn format_ops(v f64) string {
	n := i64(v)
	s := '${n}'
	if s.len <= 3 {
		return s
	}
	mut buf := []u8{cap: s.len + s.len / 3}
	rem := s.len % 3
	for i, c in s.bytes() {
		if i > 0 && (i - rem) % 3 == 0 && (rem != 0 || i > 0) {
			buf << `,`
		}
		buf << c
	}
	return buf.bytestr()
}

fn print_results(results []BenchResult) {
	sep := '='.repeat(80)
	dash := '-'.repeat(80)
	println('\n${sep}')
	title := '${' '.repeat(20)}V REGEX BENCHMARK RESULTS'
	println(title)
	println(sep)
	header := '${fmt_col('Benchmark', 30)} ${fmt_col('Engine', 16)} ${fmt_col('ns/op',
		12)} ${fmt_col('ops/sec', 14)}'
	println(header)
	println(dash)

	mut current_name := ''
	for r in results {
		if r.name != current_name {
			if current_name != '' {
				println(dash)
			}
			current_name = r.name
		}
		ops_str := format_ops(r.ops_sec)
		line := '${fmt_col(r.name, 30)} ${fmt_col(r.engine, 16)} ${fmt_col('${r.ns_per_op}',
			12)} ${fmt_col(ops_str, 14)}'
		println(line)
	}
	println(sep)
	println('Iterations: ${iterations}, Warmup: ${warmup}')
}

fn main() {
	short_text := 'The quick brown fox jumps over the lazy dog 12345'
	medium_text := 'The quick brown fox jumps over the lazy dog 12345. ' +
		'Pack my box with five dozen liquor jugs. ' + 'How vexingly quick daft zebras jump! ' +
		'The five boxing wizards jump quickly. ' + 'Sphinx of black quartz, judge my vow. ' +
		'Two driven jocks help fax my big quiz.'
	long_text := medium_text.repeat(20)
	email_text := 'Contact us at user@example.com or admin@test.org for info. ' +
		'Also try support@company.co.uk and test.user+tag@domain.com please.'

	mut results := []BenchResult{}

	// === Benchmark 1: Literal Match ===
	{
		mut re_orig := regex.regex_opt(r'quick') or { panic(err) }
		re_pcre := pcre.compile(r'quick') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find(short_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find(short_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '1. Literal match (short)'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			_ = re_pcre.find(short_text) or { pcre.Match{} }
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = re_pcre.find(short_text) or { pcre.Match{} }
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '1. Literal match (short)'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 2: Character Class + Quantifier ===
	{
		mut re_orig := regex.regex_opt(r'[a-zA-Z]+') or { panic(err) }
		re_pcre := pcre.compile(r'[a-zA-Z]+') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find(medium_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find(medium_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '2. Char class [a-zA-Z]+'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			_ = re_pcre.find(medium_text) or { pcre.Match{} }
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = re_pcre.find(medium_text) or { pcre.Match{} }
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '2. Char class [a-zA-Z]+'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 3: Alternation ===
	{
		mut re_orig := regex.regex_opt(r'fox|dog|cat|bird') or { panic(err) }
		re_pcre := pcre.compile(r'fox|dog|cat|bird') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find(short_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find(short_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '3. Alternation (4 words)'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			_ = re_pcre.find(short_text) or { pcre.Match{} }
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = re_pcre.find(short_text) or { pcre.Match{} }
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '3. Alternation (4 words)'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 4: Find All (digits) ===
	{
		mut re_orig := regex.regex_opt(r'\d+') or { panic(err) }
		re_pcre := pcre.compile(r'\d+') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find_all(medium_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find_all(medium_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '4. Find all digits'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			re_pcre.find_all(medium_text)
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			re_pcre.find_all(medium_text)
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '4. Find all digits'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 5: Groups + Quantifier ===
	{
		mut re_orig := regex.regex_opt(r'(\w+)\s+(\w+)') or { panic(err) }
		re_pcre := pcre.compile(r'(\w+)\s+(\w+)') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find(medium_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find(medium_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      r'5. Groups (\w+)\s+(\w+)'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			_ = re_pcre.find(medium_text) or { pcre.Match{} }
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = re_pcre.find(medium_text) or { pcre.Match{} }
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      r'5. Groups (\w+)\s+(\w+)'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 6: Email-like pattern ===
	{
		mut re_orig := regex.regex_opt(r'[\w.]+@[\w]+\.[\w]+') or { panic(err) }
		re_pcre := pcre.compile(r'[\w.]+@[\w]+\.[\w]+') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find_all(email_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find_all(email_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '6. Email pattern'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			re_pcre.find_all(email_text)
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			re_pcre.find_all(email_text)
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '6. Email pattern'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 7: Long text scan ===
	{
		mut re_orig := regex.regex_opt(r'quickly') or { panic(err) }
		re_pcre := pcre.compile(r'quickly') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.find_all(long_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.find_all(long_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '7. Long text literal scan'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			re_pcre.find_all(long_text)
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			re_pcre.find_all(long_text)
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '7. Long text literal scan'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 8: Replace all ===
	{
		mut re_orig := regex.regex_opt(r'\d+') or { panic(err) }
		re_pcre := pcre.compile(r'\d+') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.replace_simple(medium_text, 'NUM')
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.replace_simple(medium_text, 'NUM')
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '8. Replace all digits'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			re_pcre.replace_all(medium_text, 'NUM')
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			re_pcre.replace_all(medium_text, 'NUM')
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '8. Replace all digits'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 9: Dot-star greedy ===
	{
		mut re_orig := regex.regex_opt(r'.*fox.*dog') or { panic(err) }
		re_pcre := pcre.compile(r'.*fox.*dog') or { panic(err) }

		for _ in 0 .. warmup {
			re_orig.match_string(short_text)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_orig.match_string(short_text)
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '9. Dot-star greedy'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			_ = re_pcre.fullmatch(short_text) or { pcre.Match{} }
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = re_pcre.fullmatch(short_text) or { pcre.Match{} }
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '9. Dot-star greedy'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === Benchmark 10: Compilation speed ===
	{
		for _ in 0 .. warmup {
			_ = regex.regex_opt(r'\w+@\w+\.\w+') or { regex.RE{} }
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = regex.regex_opt(r'\w+@\w+\.\w+') or { regex.RE{} }
		}
		elapsed := sw.elapsed()
		ns1 := elapsed.nanoseconds() / iterations
		ops1 := if ns1 > 0 { 1_000_000_000.0 / f64(ns1) } else { 0.0 }
		results << BenchResult{
			name:      '10. Compile simple'
			engine:    'V-Original'
			ns_per_op: ns1
			ops_sec:   ops1
		}

		for _ in 0 .. warmup {
			_ = pcre.compile(r'\w+@\w+\.\w+') or { pcre.Regex{} }
		}
		sw = time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = pcre.compile(r'\w+@\w+\.\w+') or { pcre.Regex{} }
		}
		elapsed2 := sw.elapsed()
		ns2 := elapsed2.nanoseconds() / iterations
		ops2 := if ns2 > 0 { 1_000_000_000.0 / f64(ns2) } else { 0.0 }
		results << BenchResult{
			name:      '10. Compile simple'
			engine:    'V-PCRE'
			ns_per_op: ns2
			ops_sec:   ops2
		}
	}

	// === PCRE-only: Lookahead ===
	{
		re_pcre := pcre.compile(r'\w+(?=\.)') or { panic(err) }

		for _ in 0 .. warmup {
			_ = re_pcre.find(email_text) or { pcre.Match{} }
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			_ = re_pcre.find(email_text) or { pcre.Match{} }
		}
		elapsed := sw.elapsed()
		ns := elapsed.nanoseconds() / iterations
		ops := if ns > 0 { 1_000_000_000.0 / f64(ns) } else { 0.0 }
		results << BenchResult{
			name:      r'11. Lookahead \w+(?=\.)'
			engine:    'V-PCRE only'
			ns_per_op: ns
			ops_sec:   ops
		}
	}

	// === PCRE-only: Backreference ===
	{
		doubled := 'the the quick quick fox fox jumps over the lazy dog'
		re_pcre := pcre.compile(r'(\w+)\s+\1') or { panic(err) }

		for _ in 0 .. warmup {
			re_pcre.find_all(doubled)
		}
		mut sw := time.new_stopwatch()
		for _ in 0 .. iterations {
			re_pcre.find_all(doubled)
		}
		elapsed := sw.elapsed()
		ns := elapsed.nanoseconds() / iterations
		ops := if ns > 0 { 1_000_000_000.0 / f64(ns) } else { 0.0 }
		results << BenchResult{
			name:      r'12. Backref (\w+)\s+\1'
			engine:    'V-PCRE only'
			ns_per_op: ns
			ops_sec:   ops
		}
	}

	print_results(results)
}
