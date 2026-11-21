module netcode

import gg
import math

// Snapshot is one timestamped world state used by replication.
pub struct Snapshot {
pub mut:
	time       f32
	positions  []gg.Vec3
	velocities []gg.Vec3
}

pub fn new_snapshot(time f32, count int) Snapshot {
	return Snapshot{
		time:       time
		positions:  []gg.Vec3{len: count}
		velocities: []gg.Vec3{len: count}
	}
}

// interpolate blends two snapshots for render-time smoothing.
pub fn interpolate(a Snapshot, b Snapshot, t f32) Snapshot {
	count := math.min(a.positions.len, b.positions.len)
	mut out := new_snapshot(a.time + (b.time - a.time) * t, count)
	k := math.clip(t, 0, 1)
	for i in 0 .. count {
		out.positions[i] = a.positions[i] + (b.positions[i] - a.positions[i]).scale(k)
		if i < a.velocities.len && i < b.velocities.len {
			out.velocities[i] = a.velocities[i] + (b.velocities[i] - a.velocities[i]).scale(k)
		}
	}
	return out
}

// SnapshotBuffer keeps a short history for interpolation and reconciliation.
pub struct SnapshotBuffer {
pub mut:
	items    []Snapshot
	capacity int = 32
}

pub fn new_snapshot_buffer(capacity int) SnapshotBuffer {
	return SnapshotBuffer{ capacity: if capacity < 2 { 2 } else { capacity } }
}

pub fn (mut b SnapshotBuffer) push(s Snapshot) {
	b.items << s
	for b.items.len > b.capacity {
		b.items = b.items[1..]
	}
}

// sample returns two adjacent snapshots and the blend factor for time t.
pub fn (b &SnapshotBuffer) sample(t f32) ?(Snapshot, Snapshot, f32) {
	if b.items.len == 0 {
		return none
	}
	if b.items.len == 1 || t <= b.items[0].time {
		return b.items[0], b.items[0], 0
	}
	for i in 1 .. b.items.len {
		if t <= b.items[i].time {
			span := b.items[i].time - b.items[i - 1].time
			frac := if span > 0 { (t - b.items[i - 1].time) / span } else { f32(0) }
			return b.items[i - 1], b.items[i], math.clip(frac, 0, 1)
		}
	}
	last := b.items[b.items.len - 1]
	return last, last, 0
}
