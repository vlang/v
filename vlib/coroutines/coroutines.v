// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module coroutines

import time

#flag -I @VEXEROOT/thirdparty/photon
#flag @VEXEROOT/thirdparty/photon/photonwrapper.so

#include "photonwrapper.h"

fn C.photon_init_default() int
fn C.photon_thread_create(f voidptr)
fn C.photon_sleep_s(n int)
fn C.photon_sleep_ms(n int)

// sleep is coroutine-safe version of time.sleep()
pub fn sleep(duration time.Duration) {
	C.photon_sleep_ms(duration.milliseconds())
}

fn init() {
	C.photon_init_default()
}
