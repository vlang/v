module time

#flag -lws2_32
#include <Timezoneapi.h>

// Windows's TimeZone API.
struct C.TIME_ZONE_INFORMATION{
	Bias 		 int
	StandardBias int
	DaylightBias int
	StandardDate C.SYSTEMTIME
	DaylightName C.SYSTEMTIME
}

struct C.SYSTEMTIME {
	wYear u16
	wMonth u16
	wDayOfWeek u16
	wDay u16
	wHour u16 
	wMinute u16
	wSecond u16
	wMillseconds u16
}

fn C.SetTimeZoneInfomation(tz C.TIME_ZONE_INFORMATION) bool
fn C.GetTimeZoneInfomation(tz *C.TIME_ZONE_INFORMATION) bool

pub fn() get_system_timezone(){
	
}