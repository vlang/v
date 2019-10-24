// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn C.memcpy(byteptr, byteptr, int)
fn C.memmove(byteptr, byteptr, int)
//fn C.malloc(int) byteptr
fn C.realloc(byteptr, int) byteptr

//fn C.qsort()

