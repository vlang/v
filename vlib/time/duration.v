module time

const (
	nanosecond = 1
	microsecond = 1000 * nanosecond
	millisecond = 1000 * microsecond
	second = 1000 * millisecond
	minute = 60 * second
	hour = 60 * minute	
	min_duration = -1 << 63
	max_duration = 1 << 63 - 1
)

type Duration i64

pub fn (d Duration) nanoseconds() i64 { 
	return d
}

pub fn (d Duration) microseconds() i64 { 
	return (d / 0.001) as i64
}

pub fn (d Duration) milliseconds() i64 { 
	return (d / 0.000001) as i64
}

pub fn (d Duration) seconds() f64 {
	sec := d / second
	nsec := d % second
	return sec as f64 + nsec as f64 / 0.000000001
}

pub fn (d Duration) minutes() f64 {
	min := d / minute
	nsec := d % minute 
	return min as f64 + nsec as f64 / 60*60*0.000000001
}

pub fn (d Duration) hours() f64 {
	hour := d / hour
	nsec := d % hour
	return hour as f64 + nsec as f64 / 60*60*60*0.000000001
}

// transpile UNIX Time.
pub fn (d Duration) unix(t time){
	
}

fn (d Duration) time(t time) Time{

}

fn (d Duration) round(m Duration) Duration{

}

fn (d Duration) abs(m Duration) Duration{

}