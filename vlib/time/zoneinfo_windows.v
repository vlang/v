module time

#flag -lws2_32
#include <Timezoneapi.h>

struct TimeZoneInfomation{
	Bias 		 int
	StandardName [32]u16
	StandardDate C.SYSTEMTIME
	StandardBias int
	DaylightName [32]u16
	DaylightName C.SYSTEMTIME
	DaylightBias int
}

fn SetTimeZoneInfomation(TIME_ZONE_INFORMATION *C.lpTimeZoneInfomation) bool
fn GetTimeZoneInfomation(LPTIME_ZONE_INFORMATION *C.lpTimeZoneInfomation) 

pub fn() get_system_timezone(){
	
}