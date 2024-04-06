// Note: the name zz_epoll_data is deliberately chosen to minimise the chance of conflicts with `epoll_data` in the future.
typedef union zz_epoll_data {
     void *ptr;
     int fd;
     uint32_t u32;
     uint64_t u64;
} zz_epoll_data_t;
 
struct zz_epoll_event {
    uint32_t events; /* Epoll events */
    zz_epoll_data_t data; /* User data variable */
};
