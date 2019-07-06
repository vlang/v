// Ported based on https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-go-3.html
// Output:
// -0.169075164
// -0.169059907

// Elapsed time: 4.91s
// Maximum resident (kbytes): 788
// Between Rust and Go

import math

const (
    SolarMass   = 39.47841760435743197 //4.0 * math.Pi * math.Pi
    DaysPerYear = 365.24
    N           = 5
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

fn advance(sys System, dt f64) {
    for i := 0; i < N - 1; i++ {
        mut _vx := sys.v[i].x
        mut _vy := sys.v[i].y
        mut _vz := sys.v[i].z

        for j := i + 1; j < N; j++ {
            dx := sys.s[i].x - sys.s[j].x
            dy := sys.s[i].y - sys.s[j].y
            dz := sys.s[i].z - sys.s[j].z

            dSquared := dx * dx + dy * dy + dz * dz
            distance := math.sqrt(dSquared)
            mag := (dt / (dSquared * distance))
            mi := sys.v[i].m

            _vx -= dx * sys.v[j].m * mag
            _vy -= dy * sys.v[j].m * mag
            _vz -= dz * sys.v[j].m * mag

            sys.v[j].x += dx * mi * mag
            sys.v[j].y += dy * mi * mag
            sys.v[j].z += dz * mi * mag
        }
        sys.v[i].x = _vx
        sys.v[i].y = _vy
        sys.v[i].z = _vz
    }

    for i := 0; i < N; i++ {
        sys.s[i].x += dt * sys.v[i].x
        sys.s[i].y += dt * sys.v[i].y
        sys.s[i].z += dt * sys.v[i].z
    }
}

fn offsetmomentum(sys System) {
    mut px := f64(0)
    mut py := f64(0)
    mut pz := f64(0)

    for i := 0; i < N; i++ {
        px += sys.v[i].x * sys.v[i].m
        py += sys.v[i].y * sys.v[i].m
        pz += sys.v[i].z * sys.v[i].m
    }
    sys.v[0].x = -px / SolarMass
    sys.v[0].y = -py / SolarMass
    sys.v[0].z = -pz / SolarMass
}

fn energy(sys System) f64 {
    mut e := f64(0)
    for i := 0; i < N; i++ {
        e += 0.5 * sys.v[i].m * (sys.v[i].x * sys.v[i].x + sys.v[i].y * sys.v[i].y + sys.v[i].z * sys.v[i].z)
        for j := i + 1; j < N; j++ {
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
    mut x := []Momentum
    x << Momentum {0.0, 0.0, 0.0, SolarMass}
    x << Momentum {1.66007664274403694e-03 * DaysPerYear, 7.69901118419740425e-03 * DaysPerYear, -6.90460016972063023e-05 * DaysPerYear, 9.54791938424326609e-04 * SolarMass}
    x << Momentum {-2.76742510726862411e-03 * DaysPerYear, 4.99852801234917238e-03 * DaysPerYear, 2.30417297573763929e-05 * DaysPerYear, 2.85885980666130812e-04 * SolarMass}
    x << Momentum {2.96460137564761618e-03 * DaysPerYear, 2.37847173959480950e-03 * DaysPerYear, -2.96589568540237556e-05 * DaysPerYear, 4.36624404335156298e-05 * SolarMass}
    x << Momentum {2.68067772490389322e-03 * DaysPerYear, 1.62824170038242295e-03 * DaysPerYear, -9.51592254519715870e-05 * DaysPerYear, 5.15138902046611451e-05 * SolarMass}
    return x
}

pub fn arr_position() []Position {
    mut x := []Position
    x << Position {0.0, 0.0, 0.0}
    x << Position {4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01}
    x << Position {8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01}
    x << Position {1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01} 
    x << Position {1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01}
    return x
}

fn main() {

sys := &System {arr_momentum(), arr_position()}
offsetmomentum(sys)

println('${energy(sys):.9f}')  //-0.169075164
for i := 0; i < 50000000; i++ {
    advance(sys, 0.01)
}
println('${energy(sys):.9f}') //-0.169059907

}
