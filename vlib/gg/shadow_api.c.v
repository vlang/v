// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// set_shadow_map attaches sm to ctx so subsequent material-mesh draws are
// shadowed. Pass none to disable shadows. The ShadowMap3D must stay alive
// while attached.
pub fn (ctx &Context) set_shadow_map(sm ?&ShadowMap3D) {
	mut ctx_mut := unsafe { &Context(ctx) }
	ctx_mut.shadow_map = sm
}

// clear_shadow_map disables shadow mapping for subsequent draws.
pub fn (ctx &Context) clear_shadow_map() {
	mut ctx_mut := unsafe { &Context(ctx) }
	ctx_mut.shadow_map = none
}
