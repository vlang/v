# sketches

Experimental probabilistic counting structures adjacent to `x.lsh`.

- `HyperLogLog`: distinct-value cardinality estimation with small-range
  correction and register merging.
- `CountMin`: frequency estimation that never underestimates.
- `SpaceSaving`: bounded-memory heavy hitters with `top(k)`.

## Quick start

```v
import x.sketches

fn main() {
	mut hll := sketches.new_hyperloglog(12, 42)!
	hll.add('alice')
	hll.add('bob')
	hll.add('alice')
	println(hll.count())

	mut cm := sketches.new_countmin(1024, 4, 42)!
	cm.add('alice', 1)
	println(cm.estimate('alice'))
}
```

Estimates are approximate. HyperLogLog precision must be in 4..20; higher
precision uses more memory but lowers error. `CountMin` and `SpaceSaving` need
width, depth and capacity chosen for the expected stream size.

This module is experimental and may change or move out of vlib later.
