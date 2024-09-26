fn mkey[K, V](m map[K]V) K {
	return K{}
}

fn mvalue[K, V](m map[K]V) V {
	return V{}
}

fn aelem[E](a []E) E {
	return E{}
}

fn g[T](x T) {
	$if T is $map {
		dk := mkey(x)
		dv := mvalue(x)
		eprintln('default k: `${dk}` | typeof dk: ${typeof(dk).name}')
		eprintln('default v: `${dv}` | typeof dv: ${typeof(dv).name}')
		for k, v in x {
			eprintln('> k: ${k} | v: ${v}')
		}
	}
	$if T is $array {
		de := aelem(x)
		eprintln('default e: `${de}` | typeof de: ${typeof(de).name}')
		for idx, e in x {
			eprintln('> idx: ${idx} | e: ${e}')
		}
	}
}

fn test_main() {
	g({
		'abc': 123
		'def': 456
	})
	g([1, 2, 3])
	g({
		123: 'ggg'
		456: 'hhh'
	})
	g(['xyz', 'zzz'])
}
