module time
// Include Windows API.
#include <windows.h>
#include <timezoneapi.h>

struct C.TIME_ZONE_INFORMATION{
	C.Bias 		   C.ULONG
	C.StandardBias C.ULONG
	C.DatlightBias C.ULONG
	C.StandardDate C.SYSTEMTIME
	C.DaylightDate C.SYSTEMTIME
}

struct C.lpTimeZoneInfomation{
	C.Bias 		   C.ULONG
	C.StandardBias C.ULONG
	C.DatlightBias C.ULONG
	C.StandardDate C.SYSTEMTIME
	C.DaylightDate C.SYSTEMTIME
}

fn C.SetTimeZoneInfomation(TIME_ZONE_INFORMATION *C.lpTimeZoneInfomation) C.BOOL
fn C.GetTimeZoneInfomation(LPTIME_ZONE_INFORMATION *C.lpTimeZoneInfomation) C.DWORD

pub fn() get_system_timezone(){
	
}