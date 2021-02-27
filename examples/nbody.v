// Ported based on https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-go-3.html
// Output:
// -0.169075164
// -0.169059907
import math

const (
	solar_mass    = 39.47841760435743197 // 4.0 * math.Pi * math.Pi
	days_per_year = 365.24
	c_n           = 5
)

struct Position {
pub mut:
	x f64
	y f64
	z f64
}

struct Momentum {
pub mut:
	x f64
	y f64
	z f64
	m f64
}

struct System {
pub mut:
	v []Momentum
	s []Position
}

fn advance(mut sys System, dt f64) {
	for i in 0 .. c_n - 1 {
		mut vx := sys.v[i].x
		mut vy := sys.v[i].y
		mut vz := sys.v[i].z

		for j := i + 1; j < c_n; j++ {
			dx := sys.s[i].x - sys.s[j].x
			dy := sys.s[i].y - sys.s[j].y
			dz := sys.s[i].z - sys.s[j].z

			dsquared := dx * dx + dy * dy + dz * dz
			distance := math.sqrt(dsquared)
			mag := (dt / (dsquared * distance))
			mi := sys.v[i].m

			vx -= dx * sys.v[j].m * mag
			vy -= dy * sys.v[j].m * mag
			vz -= dz * sys.v[j].m * mag

			sys.v[j].x += dx * mi * mag
			sys.v[j].y += dy * mi * mag
			sys.v[j].z += dz * mi * mag
		}
		sys.v[i].x = vx
		sys.v[i].y = vy
		sys.v[i].z = vz
	}

	for i in 0 .. c_n {
		sys.s[i].x += dt * sys.v[i].x
		sys.s[i].y += dt * sys.v[i].y
		sys.s[i].z += dt * sys.v[i].z
	}
}

fn offsetmomentum(mut sys System) {
	mut px := f64(0)
	mut py := f64(0)
	mut pz := f64(0)

	for i in 0 .. c_n {
		px += sys.v[i].x * sys.v[i].m
		py += sys.v[i].y * sys.v[i].m
		pz += sys.v[i].z * sys.v[i].m
	}
	sys.v[0].x = -px / solar_mass
	sys.v[0].y = -py / solar_mass
	sys.v[0].z = -pz / solar_mass
}

fn energy(sys System) f64 {
	mut e := f64(0)
	for i in 0 .. c_n {
		e += 0.5 * sys.v[i].m * (sys.v[i].x * sys.v[i].x + sys.v[i].y * sys.v[i].y +
			sys.v[i].z * sys.v[i].z)
		for j := i + 1; j < c_n; j++ {
			dx := sys.s[i].x - sys.s[j].x
			dy := sys.s[i].y - sys.s[j].y
			dz := sys.s[i].z - sys.s[j].z
			distance := math.sqrt(dx * dx + dy * dy + dz * dz)
			e -= (sys.v[i].m * sys.v[j].m) / distance
		}
	}
	return e
}

fn arr_momentum() []Momentum {
	return [
		Momentum{0.0, 0.0, 0.0, solar_mass},
		Momentum{1.66007664274403694e-03 * days_per_year, 7.69901118419740425e-03 * days_per_year, -6.90460016972063023e-05 * days_per_year, 9.54791938424326609e-04 * solar_mass},
		Momentum{-2.76742510726862411e-03 * days_per_year, 4.99852801234917238e-03 * days_per_year, 2.30417297573763929e-05 * days_per_year, 2.85885980666130812e-04 * solar_mass},
		Momentum{2.96460137564761618e-03 * days_per_year, 2.37847173959480950e-03 * days_per_year, -2.96589568540237556e-05 * days_per_year, 4.36624404335156298e-05 * solar_mass},
		Momentum{2.68067772490389322e-03 * days_per_year, 1.62824170038242295e-03 * days_per_year, -9.51592254519715870e-05 * days_per_year, 5.15138902046611451e-05 * solar_mass},
	]
}

fn arr_position() []Position {
	return [
		Position{0.0, 0.0, 0.0},
		Position{4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01},
		Position{8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01},
		Position{1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01},
		Position{1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01},
	]
}

fn main() {
	mut sys := &System{arr_momentum(), arr_position()}
	offsetmomentum(mut sys)
	println('${energy(sys):.9f}') //-0.169075164
	for _ in 0 .. 50_000_000 {
		advance(mut sys, 0.01)
	}
	println('${energy(sys):.9f}') //-0.169059907
}
