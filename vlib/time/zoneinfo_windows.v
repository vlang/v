module time

#flag -lws2_32
#include <Timezoneapi.h>

// Windows's TimeZone API.
struct TimeZoneInfomation{
	Bias 		 u32
	StandardBias u32
	DaylightBias u32
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

fn C.SetTimeZoneInfomation(TIME_ZONE_INFORMATION *C.lpTimeZoneInfomation)
fn C.GetTimeZoneInfomation(LPTIME_ZONE_INFORMATION *C.lpTimeZoneInfomation) 

pub fn() get_system_timezone(){
	
}