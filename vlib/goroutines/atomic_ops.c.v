// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Atomic operations wrapper for the goroutine scheduler.
// Uses C11 stdatomic for portable atomic operations.
module goroutines

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

fn C.atomic_fetch_add(ptr &i32, val i32) i32
fn C.atomic_fetch_sub(ptr &i32, val i32) i32
fn C.atomic_load(ptr &u32) u32
fn C.atomic_store(ptr &u32, val u32)
fn C.atomic_compare_exchange_strong(ptr voidptr, expected voidptr, desired voidptr) bool

fn C.memcpy(dest voidptr, src voidptr, n usize) voidptr
fn C.memset(dest voidptr, ch int, n usize) voidptr
fn C.rand() int
