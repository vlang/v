module time

pub type ZoneinfoLoaderFn = fn (name string) ![]u8

// Zone describes one time zone rule in an IANA location.
pub struct Zone {
pub:
	name   string
	offset int
	is_dst bool
}

// Location contains parsed IANA time zone data.
@[heap]
pub struct Location {
pub:
	name string
mut:
	zones       []Zone
	transitions []ZoneTransition
	posix       PosixZoneRule
	has_posix   bool
}

struct ZoneTransition {
	when  i64
	index int
}

struct PosixZoneRule {
	std_name   string
	std_offset int
	dst_name   string
	dst_offset int
	start      PosixRule
	end        PosixRule
	has_dst    bool
}

enum PosixRuleKind {
	month_week_day
	julian_no_leap
	day_of_year
}

struct PosixRule {
	kind    PosixRuleKind
	month   int
	week    int
	weekday int
	day     int
	seconds int
}
