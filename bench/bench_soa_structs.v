// bench_soa_structs.v - V2 benchmark for @[soa] structs
// Build and run with:
//   ./cmd/v2/v2 -prod -backend cleanc bench/bench_soa_structs.v -o bench/bench_soa_structs
//   ./bench/bench_soa_structs
module main

import time

const particle_count = 500_000
const build_rounds = 5
const sum_rounds = 48
const hot_fields_rounds = 40
const full_struct_rounds = 20
const integrate_rounds = 20

@[soa]
struct Particle {
mut:
	x           f32
	y           f32
	z           f32
	vx          f32
	vy          f32
	vz          f32
	mass        f32
	temperature f32
	ax          f32
	ay          f32
	az          f32
	life        f32
	drag        f32
	color_r     f32
	color_g     f32
	color_b     f32
}

@[typedef]
struct C.Particle_SOA {
	len         int
	cap         int
	x           &f32
	y           &f32
	z           &f32
	vx          &f32
	vy          &f32
	vz          &f32
	mass        &f32
	temperature &f32
	ax          &f32
	ay          &f32
	az          &f32
	life        &f32
	drag        &f32
	color_r     &f32
	color_g     &f32
	color_b     &f32
}

fn C.Particle_SOA_new(int, int) C.Particle_SOA
fn C.Particle_SOA_push(&C.Particle_SOA, Particle)
fn C.Particle_SOA_get(C.Particle_SOA, int) Particle
fn C.Particle_SOA_free(&C.Particle_SOA)

struct BenchResult {
	elapsed_ms i64
	checksum   f64
}

fn make_particle(i int) Particle {
	base := f32(i % 1024)
	return Particle{
		x:           base * 0.25
		y:           base * 0.5
		z:           base * 0.75
		vx:          f32(0.001) * f32((i % 13) + 1)
		vy:          f32(0.0015) * f32((i % 17) + 1)
		vz:          f32(0.0005) * f32((i % 19) + 1)
		mass:        f32(1.0) + f32(i % 5) * f32(0.1)
		temperature: f32(20.0) + f32(i % 7)
		ax:          f32(0.0001) * f32((i % 11) + 1)
		ay:          f32(0.0002) * f32((i % 9) + 1)
		az:          f32(0.0003) * f32((i % 7) + 1)
		life:        f32(100.0) - f32(i % 80)
		drag:        f32(0.98)
		color_r:     f32(i % 255) / f32(255.0)
		color_g:     f32((i * 3) % 255) / f32(255.0)
		color_b:     f32((i * 7) % 255) / f32(255.0)
	}
}

fn build_aos() []Particle {
	mut particles := []Particle{cap: particle_count}
	for i in 0 .. particle_count {
		particles << make_particle(i)
	}
	return particles
}

fn build_soa() C.Particle_SOA {
	mut soa := C.Particle_SOA_new(0, particle_count)
	for i in 0 .. particle_count {
		C.Particle_SOA_push(&soa, make_particle(i))
	}
	return soa
}

fn build_soa_indexed() C.Particle_SOA {
	mut soa := C.Particle_SOA_new(particle_count, particle_count)
	unsafe {
		for i in 0 .. particle_count {
			p := make_particle(i)
			soa.x[i] = p.x
			soa.y[i] = p.y
			soa.z[i] = p.z
			soa.vx[i] = p.vx
			soa.vy[i] = p.vy
			soa.vz[i] = p.vz
			soa.mass[i] = p.mass
			soa.temperature[i] = p.temperature
			soa.ax[i] = p.ax
			soa.ay[i] = p.ay
			soa.az[i] = p.az
			soa.life[i] = p.life
			soa.drag[i] = p.drag
			soa.color_r[i] = p.color_r
			soa.color_g[i] = p.color_g
			soa.color_b[i] = p.color_b
		}
	}
	return soa
}

fn bench_build_aos() BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. build_rounds {
		particles := build_aos()
		sum += particles.len
		sum += particles[0].x
		sum += particles[particles.len - 1].life
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_build_soa() BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. build_rounds {
		mut soa := build_soa()
		sum += soa.len
		unsafe {
			sum += soa.x[0]
			sum += soa.life[soa.len - 1]
		}
		C.Particle_SOA_free(&soa)
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_build_soa_indexed() BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. build_rounds {
		mut soa := build_soa_indexed()
		sum += soa.len
		unsafe {
			sum += soa.x[0]
			sum += soa.life[soa.len - 1]
		}
		C.Particle_SOA_free(&soa)
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_sum_x_aos(particles []Particle) BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. sum_rounds {
		for i in 0 .. particles.len {
			sum += particles[i].x
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_sum_x_soa(soa C.Particle_SOA) BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. sum_rounds {
		unsafe {
			for i in 0 .. soa.len {
				sum += soa.x[i]
			}
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_sum_hot_fields_aos(particles []Particle) BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. hot_fields_rounds {
		for i in 0 .. particles.len {
			sum += particles[i].x + particles[i].y + particles[i].z + particles[i].life
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_sum_hot_fields_soa(soa C.Particle_SOA) BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. hot_fields_rounds {
		unsafe {
			for i in 0 .. soa.len {
				sum += soa.x[i] + soa.y[i] + soa.z[i] + soa.life[i]
			}
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_sum_all_fields_aos(particles []Particle) BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. full_struct_rounds {
		for i in 0 .. particles.len {
			sum += particles[i].x + particles[i].y + particles[i].z + particles[i].vx +
				particles[i].vy + particles[i].vz + particles[i].mass + particles[i].temperature +
				particles[i].ax + particles[i].ay + particles[i].az + particles[i].life +
				particles[i].drag + particles[i].color_r + particles[i].color_g +
				particles[i].color_b
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_sum_all_fields_soa(soa C.Particle_SOA) BenchResult {
	sw := time.new_stopwatch()
	mut sum := f64(0.0)
	for _ in 0 .. full_struct_rounds {
		for i in 0 .. soa.len {
			p := C.Particle_SOA_get(soa, i)
			sum += p.x + p.y + p.z + p.vx + p.vy + p.vz + p.mass + p.temperature + p.ax + p.ay +
				p.az + p.life + p.drag + p.color_r + p.color_g + p.color_b
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_integrate_aos(mut particles []Particle) BenchResult {
	sw := time.new_stopwatch()
	for _ in 0 .. integrate_rounds {
		for i in 0 .. particles.len {
			particles[i].vx += particles[i].ax
			particles[i].vy += particles[i].ay
			particles[i].vz += particles[i].az
			particles[i].x += particles[i].vx * particles[i].mass
			particles[i].y += particles[i].vy * particles[i].mass
			particles[i].z += particles[i].vz * particles[i].mass
			particles[i].life -= particles[i].drag
		}
	}
	mut sum := f64(0.0)
	for i in 0 .. particles.len {
		sum += particles[i].x + particles[i].y + particles[i].z + particles[i].life
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn bench_integrate_soa(mut soa C.Particle_SOA) BenchResult {
	sw := time.new_stopwatch()
	unsafe {
		for _ in 0 .. integrate_rounds {
			for i in 0 .. soa.len {
				soa.vx[i] += soa.ax[i]
				soa.vy[i] += soa.ay[i]
				soa.vz[i] += soa.az[i]
				soa.x[i] += soa.vx[i] * soa.mass[i]
				soa.y[i] += soa.vy[i] * soa.mass[i]
				soa.z[i] += soa.vz[i] * soa.mass[i]
				soa.life[i] -= soa.drag[i]
			}
		}
	}
	mut sum := f64(0.0)
	unsafe {
		for i in 0 .. soa.len {
			sum += soa.x[i] + soa.y[i] + soa.z[i] + soa.life[i]
		}
	}
	return BenchResult{
		elapsed_ms: sw.elapsed().milliseconds()
		checksum:   sum
	}
}

fn print_result(name string, aos BenchResult, soa BenchResult) {
	println(name)
	println('  aos: ${aos.elapsed_ms} ms, checksum=${aos.checksum}')
	println('  soa: ${soa.elapsed_ms} ms, checksum=${soa.checksum}')
	if soa.elapsed_ms > 0 {
		println('  speedup: ${f64(aos.elapsed_ms) / f64(soa.elapsed_ms)}x')
	}
}

fn print_build_results(aos BenchResult, soa_push BenchResult, soa_indexed BenchResult) {
	println('build particles')
	println('  aos: ${aos.elapsed_ms} ms, checksum=${aos.checksum}')
	println('  soa push: ${soa_push.elapsed_ms} ms, checksum=${soa_push.checksum}')
	println('  soa indexed: ${soa_indexed.elapsed_ms} ms, checksum=${soa_indexed.checksum}')
	if soa_indexed.elapsed_ms > 0 {
		println('  push vs indexed: ${f64(soa_push.elapsed_ms) / f64(soa_indexed.elapsed_ms)}x')
		println('  indexed speedup over aos: ${f64(aos.elapsed_ms) / f64(soa_indexed.elapsed_ms)}x')
	}
}

fn main() {
	println('soa struct bench (v2 cleanc)')
	println('particle_count=${particle_count}, build_rounds=${build_rounds}, sum_rounds=${sum_rounds}, hot_fields_rounds=${hot_fields_rounds}, full_struct_rounds=${full_struct_rounds}, integrate_rounds=${integrate_rounds}')

	result_build_aos := bench_build_aos()
	result_build_soa_push := bench_build_soa()
	result_build_soa_indexed := bench_build_soa_indexed()
	print_build_results(result_build_aos, result_build_soa_push, result_build_soa_indexed)

	aos_scan := build_aos()
	soa_scan := build_soa()
	result_aos_scan := bench_sum_x_aos(aos_scan)
	result_soa_scan := bench_sum_x_soa(soa_scan)
	print_result('sum x only', result_aos_scan, result_soa_scan)

	result_aos_hot_fields := bench_sum_hot_fields_aos(aos_scan)
	result_soa_hot_fields := bench_sum_hot_fields_soa(soa_scan)
	print_result('sum x/y/z/life', result_aos_hot_fields, result_soa_hot_fields)

	result_aos_full_struct := bench_sum_all_fields_aos(aos_scan)
	result_soa_full_struct := bench_sum_all_fields_soa(soa_scan)
	print_result('materialize full struct and sum all fields', result_aos_full_struct,
		result_soa_full_struct)
	C.Particle_SOA_free(&soa_scan)

	mut aos_integrate := build_aos()
	mut soa_integrate := build_soa()
	result_aos_integrate := bench_integrate_aos(mut aos_integrate)
	result_soa_integrate := bench_integrate_soa(mut soa_integrate)
	print_result('integrate position, velocity, and life', result_aos_integrate, result_soa_integrate)
	C.Particle_SOA_free(&soa_integrate)
}
