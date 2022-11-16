module prime

import rand
import toml
import os

pub const toml_path = 'vlib/v/tests/bench/math_big_gcd/primes.toml'

pub interface DataI {
	to_primeset() PrimeSet
	from_primeset(PrimeSet) DataI
}

pub fn (di DataI) cast<T>() DataI {
	return T{}.from_primeset(di.to_primeset())
}

pub type PrimeCfg = PrimeSet

pub fn (pc PrimeCfg) short() string {
	return "r: '${pc.r}' a: '${pc.a}' b: '${pc.b}'"
}

[heap]
pub struct PrimeSet {
pub mut:
	r string [required]
	a string [required]
	b string [required]
}

pub fn (p PrimeSet) to_primeset() PrimeSet {
	return p
}

pub fn (p PrimeSet) from_primeset(ps PrimeSet) DataI {
	return ps
}

pub fn (p PrimeSet) predicate(pred fn (data PrimeSet) bool) bool {
	return pred(p)
}

pub fn (p PrimeSet) key() string {
	return [p.r, p.a, p.b].join('.')
}

pub fn (p PrimeSet) str() string {
	return [p.r, p.a, p.b].join(' ')
}

fn extract_count(s string) int {
	digits := '0123456789'.split('')
	if (s == '') || !s.split('').any(it in digits) {
		return 0
	}
	ds := s.split('').filter(it in digits)
	return ds.join('').int()
}

// sizes lists the available names for prime-number sections.
pub fn sizes() []string {
	primes := read_toml_file()
	return primes.keys()
}

// usage returns section-names and the count of available primes
//
pub fn usage() string {
	primes := read_toml_file()
	return sizes().map('${it}[..${primes[it].len}]').join('\n\t')
}

// reads the Map[string] []string from disk
// and returns the parsed content
fn read_toml_file() map[string][]string {
	fp := os.join_path(@VROOT, prime.toml_path)

	tm_doc := toml.parse_file(fp) or {
		err_msg := 'expected ${fp}'
		eprintln(err_msg)
		panic(err)
	}
	// TODO what happens if this goes wrong ?
	tm_primes := tm_doc.value('primes') as map[string]toml.Any

	msg := 'expected a map[string][]string in TOML-data ? corrupt ?'
	mut p := map[string][]string{}
	for k in tm_primes.keys() {
		p[k] = []string{}
		arr := tm_primes[k] or { panic(msg) }
		for _, elem in arr.array() {
			p[k] << elem as string
		}
	}
	return p
}

pub fn random_list(cfg []string) []string {
	primes := read_toml_file()

	mut p_list := []string{}
	match cfg.len {
		1 { // prime-size e.g. 'xs' given
			if cfg[0] !in primes {
				return p_list
			} else {
				return primes[cfg[0]]
			}
		}
		2 { // prime-size and limiter given e.g 'xs.15'
			prime_size := cfg[0]
			if prime_size !in primes {
				return p_list
			}

			mut prime_count := extract_count(cfg[1])
			if prime_count == 0 {
				return primes[prime_size]
			}
			mut num := ''
			for prime_count != 0 {
				num = primes[prime_size][rand.int_in_range(0, primes[prime_size].len - 1)]
				if num in p_list {
					continue
				}
				p_list << num
				prime_count -= 1
				if prime_count >= primes[prime_size].len {
					p_list = primes[prime_size]
					break
				}
			} // eo-for
			return p_list
		} // cfg not understood
		else {
			return p_list
		}
	}
	return p_list
}

pub fn random_set(cfg PrimeCfg) ?[]PrimeSet {
	p_lists := [
		cfg.r.split('.'),
		cfg.a.split('.'),
		cfg.b.split('.'),
	].map(random_list(it.map(it.trim_space().to_lower())))

	// test for empty lists
	//
	if p_lists.any(it.len == 0) {
		msg := [
			'bad config was :\n\n"${cfg}"',
			"maybe try e.g { r: 'l.5' a: 'xxl.5 b: 'xl.10' } makes a set of 250",
			'your config was { ${cfg.short()} }',
			'sizes ${usage()}\n',
		].join('\n\n')

		return error(msg)
	}
	// filter unique combinations thru map
	//
	mut tmp := map[string]PrimeSet{}
	for r in p_lists[0] {
		for a in p_lists[1] {
			for b in p_lists[2] {
				d := PrimeSet{r, a, b}
				if d.key() in tmp {
					continue
				}
				tmp[d.key()] = d
			}
		}
	}
	return tmp.keys().map(tmp[it])
}
