module perf

pub struct Timeline {
pub:
	entries int
}

pub struct Performance {
pub mut:
	timeline &Timeline
	limit    u32 = 250
}

// Performance.create returns a performance object with its reference field set.
pub fn Performance.create(entries int) &Performance {
	return &Performance{
		timeline: &Timeline{
			entries: entries
		}
	}
}
