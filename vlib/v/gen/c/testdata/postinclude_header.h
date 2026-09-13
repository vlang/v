#include <assert.h>

#if defined(_WIN32)
int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prev_instance, LPWSTR cmd_line, int show_cmd){
#else
int main(int argc, char *argv[]) {
#endif    
   	int res = no_main__f(42);
	assert(res == 987);
    return 0;
}
