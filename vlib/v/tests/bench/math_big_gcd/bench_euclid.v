module main

// Note: this benchmark is preferable to be compiled with: `v -prod -cg -gc boehm bench_euclid.v`
import math.big
import benchmark
import os
import v.tests.bench.math_big_gcd.prime {
	DataI,
	PrimeCfg,
	PrimeSet,
}

interface TestDataI {
	r big.Integer
	aa big.Integer
	bb big.Integer
}

type GCDSet = PrimeSet
type Clocks = map[string]benchmark.Benchmark

const (
	empty_set = GCDSet{'1', '1', '1'}
	with_dots = false
)

fn main() {
	fp := os.join_path(@VROOT, prime.toml_path)
	if !prime_file_exists(fp) {
		panic('expected file |$fp| - not found.')
	}

	mut clocks := Clocks(map[string]benchmark.Benchmark{})

	for algo in [
		'euclid',
		'gcd_binary',
		//'u32binary',
		//'u64binary'
	] {
		clocks[algo] = benchmark.new_benchmark()
	}

	// any test-prime-set needs to pass this predicate
	// before used in the benchmark.
	//
	predicate_fn := fn (ps PrimeSet) bool {
		cast_bi := bi_from_decimal_string

		r := cast_bi(ps.r)
		aa := r * cast_bi(ps.a)
		bb := r * cast_bi(ps.b)
		gcd := aa.gcd(bb)

		return if [
			gcd != big.one_int,
			gcd == bb.gcd(aa),
			gcd == r,
			aa != bb,
			(aa % gcd) == big.zero_int,
			(bb % gcd) == big.zero_int,
		].all(it == true)
		{ true } else { false }
	}

	cfgs := [
		// root-prime    a-prime      b-prime
		['s.3', 'xs.3', 's'],
		['s.10', 's.10', 'm.all'],
		['m.9', 's.10', 'xs'],
		['l.40', 'xs.10', 's.5'],
		['ml.20', 'm', 's.15'],
		['xl', 's.10', 'l.15'],
		['xxl', 'l.10', 'xl'],
		['l.30', 'm.10', 'xxl'],
		['xxl', 'xl', 'm.15'],
		['mega', 'xl.6', 's'],
		['xxl', 'xxxl.10', 'm.30'],
		['mega', 'xxxl', 'mega'],
		['giga.5', 'mega', 'giga'],
		['crazy', 'mega', 'giga'],
		['s', 'biggest', 'crazy'],
		['biggest', 'crazy', 'giga'],
	].map(PrimeCfg{it[0], it[1], it[2]})

	println('\n$cfgs.len x Tests')

	for i, prime_cfg in cfgs {
		println('\n#-${i + 1}(Stack) "$prime_cfg"')
		bench_euclid_vs_binary(prime_cfg, false, predicate_fn, mut clocks)

		// just-to-be-sure, but makes no difference in this test.
		// println('\n#-${i + 1}(Heap) "$prime_cfg"')
		// bench_euclid_vs_binary(prime_cfg, true,  predicate_fn, mut clocks)
	}
	println('')
	for _, mut clock in clocks {
		clock.stop()
	}

	println(clocks['euclid'].total_message('both algorithms '))

	msg := [
		'Seems to me as if euclid in big.Integer.gcd() performs better on ',
		'very-small-integers up to 8-byte/u64. The tests #-1..5 show this.',
		'The gcd_binary-algo seems to be perform better, the larger the numbers/buffers get.',
		'On my machine, i see consistent gains between ~10-30-percent with :',
		'\n',
		'v -prod -cg -gc boehm bench_euclid.v',
		'\n',
		'This test covers multiplied primes up-to a length of 300-char-digits in ',
		'a decimal-string. This equals (188-byte) == 47 x u32-values.',
		'edit/change primes in $prime.toml_path',
		'Improvements & critique are welcome : \n',
		'https://lemire.me/blog/2013/12/26/fastest-way-to-compute-the-greatest-common-divisor/',
	].join('\n')

	println(msg)
}

fn run_benchmark(data []DataI, heap bool, mut clocks Clocks) bool {
	mut testdata := []TestDataI{}
	for elem in data {
		// if elem is StackData || elem is HeapData{
		// 	testdata << elem
		// }
		// TODO: this reads strange
		if elem is StackData {
			testdata << elem
		}
		if elem is HeapData {
			testdata << elem
		}
	}
	// some statistics
	//
	mut tmp := []int{cap: testdata.len * 3}
	for set in testdata {
		for prime in [set.r, set.aa, set.bb] {
			bi, _ := prime.bytes()
			tmp << bi_buffer_len(bi)
		}
	}
	tmp.sort()
	min_byte := tmp.first() * 4
	max_byte := tmp.last() * 4
	mut buffer_space := 0
	for tmp.len != 0 {
		buffer_space += tmp.pop()
	}

	// trying to balance prime-size and item-count
	// minimum rounds is 100-times
	//
	mut rounds := 2000 / ((3 * buffer_space) / testdata.len)
	rounds = if rounds < 100 { 100 } else { rounds }

	ratio := (buffer_space * 4) / (testdata.len * 3)

	msg := [
		'avg-$ratio-byte/Prime, $min_byte-byte < Prime < $max_byte-byte \n',
		'~${buffer_space * 4 / 1024}-Kb-',
		if heap { 'Heap' } else { 'Stack' },
		'-space for $testdata.len-items x ',
		'$rounds-rounds',
	].join('')
	println(msg)

	mut cycles := 0
	for algo, mut clock in clocks {
		cycles = 0
		clock.step()
		for cycles < rounds {
			for set in testdata {
				match algo {
					'euclid' {
						if set.r != set.aa.gcd(set.bb) {
							eprintln('$algo failed ?')
							clock.fail()
							break
						}
					}
					'gcd_binary' {
						if set.r != set.aa.gcd_binary(set.bb) {
							eprintln('$algo failed ?')
							clock.fail()
							break
						}
					}
					else {
						eprintln('unknown algo was "$algo"')
						continue
					}
				}
			} // eo-for over testdata
			if with_dots {
				print('.')
			}
			cycles += 1
		} // eo-for cycles
		if with_dots {
			println('')
		}
		clock.ok()
		clock.measure(algo)
	} // eo-for-loop over algo, clock

	return true
}

fn bench_euclid_vs_binary(test_config PrimeCfg, heap bool, predicate_fn fn (ps PrimeSet) bool, mut clocks Clocks) bool {
	testprimes := prime.random_set(test_config) or { panic(err) }

	// validate the test-data
	//
	gcd_primes := testprimes.map(prepare_and_test_gcd(it, predicate_fn)).filter(it != empty_set)

	// just to make sure all generated primes are sane
	//
	assert gcd_primes.len == testprimes.len

	// casting the decimal-strings into big.Integers
	// here to avoid measuring string-parsing-cycles
	// during later testing.
	//
	mut casted_sets := if heap { gcd_primes.map(unsafe {
			DataI(&PrimeSet(&it)).cast<HeapData>()
		}) } else { gcd_primes.map(unsafe {
			DataI(&PrimeSet(&it)).cast<StackData>()
		}) }

	// ready use the primes in the benchmark
	//
	return run_benchmark(casted_sets, heap, mut clocks)
}

fn prepare_and_test_gcd(primeset PrimeSet, test fn (ps PrimeSet) bool) GCDSet {
	if !primeset.predicate(test) {
		eprintln('? Corrupt Testdata was ?')
		dump(primeset)
		return empty_set // {'1', '1', '1'}
	}

	cast_bi := bi_from_decimal_string

	r := cast_bi(primeset.r)
	aa := cast_bi(primeset.a) * r
	bb := cast_bi(primeset.b) * r
	gcd := aa.gcd(bb)

	return GCDSet{'$gcd', '$aa', '$bb'}
}

fn prime_file_exists(path string) bool {
	return os.is_readable(path)
}

// bi_from_decimal_string converts a string-of-decimals into
// a math.big.Integer using the big.Integers 'from_radix-fn'
//
pub fn bi_from_decimal_string(s string) big.Integer {
	return big.integer_from_radix(s, u32(10)) or {
		msg := [
			'Cannot convert prime from decimal-string.',
			'prime was : "$s"\n',
		].join('\n')
		panic(msg)
	}
}

// need the bi.digits.len - during test only - to calculate
// the size of big.Integers-buffer
//
fn bi_buffer_len(input []byte) int {
	if input.len == 0 {
		return 0
	}
	// pad input
	mut padded_input := []byte{len: ((input.len + 3) & ~0x3) - input.len, cap: (input.len + 3) & ~0x3, init: 0x0}
	padded_input << input
	mut digits := []u32{len: padded_input.len / 4}
	// combine every 4 bytes into a u32 and insert into n.digits
	for i := 0; i < padded_input.len; i += 4 {
		x3 := u32(padded_input[i])
		x2 := u32(padded_input[i + 1])
		x1 := u32(padded_input[i + 2])
		x0 := u32(padded_input[i + 3])
		val := (x3 << 24) | (x2 << 16) | (x1 << 8) | x0
		digits[(padded_input.len - i) / 4 - 1] = val
	}
	return digits.len
}

[heap]
pub struct HeapData {
pub mut:
	r  big.Integer
	aa big.Integer
	bb big.Integer
}

pub fn (hd HeapData) to_primeset() PrimeSet {
	return PrimeSet{
		r: '$hd.r'
		a: '$hd.aa'
		b: '$hd.bb'
	}
}

pub fn (hd HeapData) from_primeset(p PrimeSet) DataI {
	return DataI(HeapData{
		r: bi_from_decimal_string(p.r)
		aa: bi_from_decimal_string(p.a)
		bb: bi_from_decimal_string(p.b)
	})
}

pub struct StackData {
pub mut:
	r  big.Integer
	aa big.Integer
	bb big.Integer
}

pub fn (sd StackData) to_primeset() PrimeSet {
	return PrimeSet{
		r: '$sd.r'
		a: '$sd.aa'
		b: '$sd.bb'
	}
}

pub fn (sd StackData) from_primeset(p PrimeSet) DataI {
	return DataI(StackData{
		r: bi_from_decimal_string(p.r)
		aa: bi_from_decimal_string(p.a)
		bb: bi_from_decimal_string(p.b)
	})
}
