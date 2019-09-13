module strings 

pub fn repeat(c byte, n int) string {
	if n <= 0 {
		return ''
	}
	mut arr := malloc(n + 1)
	//mut arr := [byte(0); n + 1] 
	for i := 0; i < n; i++ {
		arr[i] = c
	}
	arr[n] = `\0`
	return string(arr, n)
}

// use levenshtein distance algorithm to calculate
// the distance between between two strings (lower is closer)
pub fn levenshtein_distance(a, b string) int {
	mut f := [int(0); b.len+1]
	for ca in a {
		mut j := 1
		mut fj1 := f[0]
		f[0]++
		for cb in b {
			mut mn := if f[j]+1 <= f[j-1]+1 { f[j]+1 } else { f[j-1]+1 }
			if cb != ca {
				mn = if mn <= fj1+1 { mn } else { fj1+1 }
			} else {
				mn = if mn <= fj1 { mn } else { fj1 }
			}
			fj1 = f[j]
			f[j] = mn
			j++
		}
	}
	return f[f.len-1]
}

// use levenshtein distance algorithm to calculate
// how similar two strings are as a percentage (higher is closer)
pub fn levenshtein_distance_percentage(a, b string) f64 {
	d := levenshtein_distance(a, b)
	l := if a.len >= b.len { a.len } else { b.len }
	return (1.00 - f64(d)/f64(l)) * 100.00
}
