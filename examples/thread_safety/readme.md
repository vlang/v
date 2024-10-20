### Run

```sh
v -prod -autofree ./queue.v -o ./queue.c && \
gcc ./queue.c -o ./queue.out && \
./queue.out
```

### Valgrind

```sh
# Helgrind: a tool for detecting synchronisation errors in programs that use the POSIX pthreads threading primitives.
valgrind --tool=helgrind ./queue.out

# DRD: a tool for detecting errors in multithreaded programs. The tool works for any program that uses the POSIX threading primitives or that uses threading concepts built on top of the POSIX threading primitives. 
valgrind --tool=drd ./queue.out 
```
