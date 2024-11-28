// TODO: convert this to v code? handle all platforms
// currently if it's not macos or win it defaults to the linux impl

static void sp_corrector(void** sp_ptr, void* tid) {
    size_t stack_size;
    char* stack_addr;
#ifdef __APPLE__
    stack_size = pthread_get_stacksize_np((pthread_t)tid);
    stack_addr = (char*) pthread_get_stackaddr_np((pthread_t)tid);
#elif defined(_WIN64)
    ULONG_PTR stack_low, stack_high;
    GetCurrentThreadStackLimits(&stack_low, &stack_high);
    stack_size = stack_high - stack_low;
    stack_addr = (char*)stack_low;
// #elif defined(__linux__)
//     pthread_attr_t gattr;
//     pthread_getattr_np((pthread_t)tid, &gattr);
//     pthread_attr_getstack(&gattr, (void**)&stack_addr, &stack_size);
//     pthread_attr_destroy(&gattr);
// #else
//     assert("unsupported platform");
#else
    pthread_attr_t gattr;
    pthread_getattr_np((pthread_t)tid, &gattr);
    pthread_attr_getstack(&gattr, (void**)&stack_addr, &stack_size);
    pthread_attr_destroy(&gattr);
#endif
    char *sp = (char*)*sp_ptr;
    if(sp <= stack_addr || sp >= stack_addr+stack_size) {
        *sp_ptr = (void*)stack_addr;
    }
}
