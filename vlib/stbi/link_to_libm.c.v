// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module stbi

// Note: stbi uses math.h because of `ldexp` and `pow`, for which we *do* have
// pure V implementations, but our `math` module still depends on libm
// because of 'powf', 'cosf', 'sinf', 'sqrtf' and 'tanf'.

// TODO: remove this file, when we have pure V implementations for the above
// functions too, and so `math` is no longer dependent on `libm` at all.

#include <math.h>

$if windows {
	$if tinyc {
		#flag @VEXEROOT/thirdparty/tcc/lib/openlibm.o
	}
} $else {
	#flag -lm
}
