#include <pthread.h>
/* This frame intentionally uses C's jump-buffer cleanup ABI. The callback
 * contains C++ objects, so forced unwinding must stop here, run the C handler,
 * then resume through the older C++ frames. */
void v_cpp_with_cleanup(void (*callback)(void *), void *argument, void (*cleanup)(void *)) {
    pthread_cleanup_push(cleanup, argument);
    callback(argument);
    pthread_cleanup_pop(0);
}
