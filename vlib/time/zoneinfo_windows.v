module time
#include <windows.h>

struct C.TIME_ZONE_INFORMATION{
	C.Bias int
	C.StandardDate C.SYSTEMTIME
}
fn C.GetTimeZoneInfomation()

fn get_system_timezone(){

}