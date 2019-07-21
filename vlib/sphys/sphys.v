module sphys

import gg

struct CollisionResponse {
	pub mut:
	collided bool
	penetration gg.Vec2
	normal   gg.Vec2
}