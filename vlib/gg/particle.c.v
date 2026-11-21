module gg

// draw renders every active particle as a filled circle using its current
// color and size. This native-only method mirrors the rest of gg's draw API.
pub fn (e &ParticleEmitter) draw(ctx &Context) {
	for p in e.particles {
		if !p.active {
			continue
		}
		ctx.draw_circle_filled(p.position.x, p.position.y, p.size, p.color)
	}
}
