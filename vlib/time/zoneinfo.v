module time

const (
	GMT = "0:00Z"
)

struct Localtion {
	name string
	zone []Zone
}

struct Zone {
	name string
	offset int
	is_dst bool
}

struct TimeZone{
	time_zone Time
}