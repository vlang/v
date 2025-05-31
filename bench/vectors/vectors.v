module main

import rand
import math

struct Vector {
	x f64
	y f64
	z f64
}

const boids_count = 10000
const max_coordinate = 10000.0
const cohesion_distance = 10.0
const separation_distance = 5.0

@[direct_array_access]
fn main() {
	mut positions := [boids_count]Vector{}
	mut velocities := [boids_count]Vector{}

	for position_index in 0 .. positions.len {
		positions[position_index] = Vector{
			x: rand.f64() * max_coordinate
			y: rand.f64() * max_coordinate
			z: rand.f64() * max_coordinate
		}
	}

	for boid_index in 0 .. positions.len {
		position := positions[boid_index]
		mut close_boids_ids := []int{}

		for other_boid_index in 0 .. positions.len {
			if boid_index == other_boid_index {
				continue
			}

			other_position := positions[other_boid_index]

			difference_x := position.x - other_position.x
			difference_y := position.y - other_position.y
			difference_z := position.z - other_position.z

			distance := difference_x * difference_x + difference_y * difference_y +
				difference_z * difference_z

			if distance <= cohesion_distance * cohesion_distance {
				close_boids_ids << other_boid_index
			}
		}

		if close_boids_ids.len == 0 {
			continue
		}

		mut cohesion := Vector{}
		mut separation := Vector{}
		mut separation_count := 0
		mut alignment := Vector{}

		for close_boid_id in close_boids_ids {
			close_boid_position := positions[close_boid_id]

			cohesion = Vector{
				x: cohesion.x + close_boid_position.x
				y: cohesion.y + close_boid_position.y
				z: cohesion.z + close_boid_position.z
			}

			difference_from_closest := Vector{
				x: position.x - close_boid_position.x
				y: position.y - close_boid_position.y
				z: position.z - close_boid_position.z
			}

			difference_magnitude := math.sqrt(
				difference_from_closest.x * difference_from_closest.x +
				difference_from_closest.y * difference_from_closest.y +
				difference_from_closest.z * difference_from_closest.z)

			if difference_magnitude <= separation_distance {
				separation = Vector{
					x: separation.x + difference_from_closest.x / difference_magnitude
					y: separation.y + difference_from_closest.y / difference_magnitude
					z: separation.z + difference_from_closest.z / difference_magnitude
				}

				separation_count += 1
			}

			close_boid_velocity := velocities[close_boid_id]

			alignment = Vector{
				x: alignment.x + close_boid_velocity.x
				y: alignment.y + close_boid_velocity.y
				z: alignment.z + close_boid_velocity.z
			}
		}

		cohesion = Vector{
			x: cohesion.x / close_boids_ids.len
			y: cohesion.y / close_boids_ids.len
			z: cohesion.z / close_boids_ids.len
		}

		cohesion_force := Vector{
			x: cohesion.x - position.x
			y: cohesion.y - position.y
			z: cohesion.z - position.z
		}

		if separation_count > 0 {
			separation = Vector{
				x: separation.x / separation_count
				y: separation.y / separation_count
				z: separation.z / separation_count
			}
		}

		alignment = Vector{
			x: alignment.x / close_boids_ids.len
			y: alignment.y / close_boids_ids.len
			z: alignment.z / close_boids_ids.len
		}

		current_velocity := velocities[boid_index]

		velocities[boid_index] = Vector{
			x: current_velocity.x + cohesion_force.x + separation.x + alignment.x
			y: current_velocity.y + cohesion_force.y + separation.y + alignment.y
			z: current_velocity.z + cohesion_force.z + separation.z + alignment.z
		}
	}

	mut position_sum := Vector{}
	mut velocity_sum := Vector{}

	for boid_index in 0 .. positions.len {
		position := positions[boid_index]
		velocity := velocities[boid_index]

		positions[boid_index] = Vector{
			x: position.x + velocity.x
			y: position.y + velocity.y
			z: position.z + velocity.z
		}

		position_sum = Vector{
			x: position_sum.x + position.x
			y: position_sum.y + position.y
			z: position_sum.z + position.z
		}

		velocity_sum = Vector{
			x: velocity_sum.x + velocity.x
			y: velocity_sum.y + velocity.y
			z: velocity_sum.z + velocity.z
		}
	}

	println('${position_sum.x} - ${position_sum.y} - ${position_sum.z}')
	println('${velocity_sum.x} - ${velocity_sum.y} - ${velocity_sum.z}')
}
