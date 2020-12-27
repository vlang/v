// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module runtime

//$if linux {
fn C.sysconf(name int) i64
//}

//$if windows {
fn C.GetCurrentProcessorNumber() u32
//}
