it is similar to dynamic_library_loading, but with higher abstraction, it is usefull for system's libraries, which haven't one determined path to the shared library, loader find first suitable path to load, and store handle.

```
import dl.loader

default_paths := [
    @mod + "/library.so",
    @mod + "/location1/library.so",
    @mod + "/location2/library.so",
    @mod + "/modules/library/library.so",    
]

mut opencl_loader := loader.get_or_create_dynamic_lib_loader(
        key: 'LibOpenCL',
        env_path: 'VCL_LIBOPENCL_PATH',
        paths: default_paths,
)!

defer {
        opencl_loader.unregister()
}

sym := opencl_loader.get_sym('add_1')!
```
for try it you can compile the library, move to other folder and export path to the library:
```
mkdir location3
v -d no_backtrace -shared modules/library/library.v -o location3/library.so
export EXAMPLE_PATH=location3
```
and try it compile and run ...