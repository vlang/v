import os

$if windows {
	#include <windows.h>
}

println(C.GetCurrentProcessId())
os.input('data:')

