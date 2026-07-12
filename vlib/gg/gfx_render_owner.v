@[has_globals]
module gg

enum GfxRenderOwner {
	none
	legacy_context
	multiwindow_app
}

struct GfxRenderOwnerState {
mut:
	kind  GfxRenderOwner
	token voidptr
}

__global gg_gfx_render_owner = GfxRenderOwnerState{}

fn gg_claim_gfx_render_owner(owner GfxRenderOwner, token voidptr) ! {
	if owner == .none {
		return
	}
	if gg_gfx_render_owner.kind == owner && gg_gfx_render_owner.token == token {
		return
	}
	if gg_gfx_render_owner.kind != .none {
		return error('gg.multiwindow: sokol.gfx is already owned by ${gg_render_owner_name(gg_gfx_render_owner.kind)}')
	}
	gg_gfx_render_owner = GfxRenderOwnerState{
		kind:  owner
		token: token
	}
}

fn gg_release_gfx_render_owner(owner GfxRenderOwner, token voidptr) {
	if owner == .none {
		return
	}
	if gg_gfx_render_owner.kind == owner && gg_gfx_render_owner.token == token {
		gg_gfx_render_owner = GfxRenderOwnerState{}
	}
}

fn gg_render_owner_name(owner GfxRenderOwner) string {
	return match owner {
		.none { 'none' }
		.legacy_context { 'gg.Context' }
		.multiwindow_app { 'gg.App' }
	}
}
