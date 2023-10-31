## Description:

`arrays` is a module that provides utility functions to make working with arrays easier.

## Examples:

```v
import arrays

fn main() {
	a := [1, 5, 7, 0, 9]
	assert arrays.min(a)! == 0
	assert arrays.max(a)! == 9
	assert arrays.idx_min(a)! == 3
}
```
