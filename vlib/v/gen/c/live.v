module c

import os
import v.flat

// set_live_source_path records the original compilation target for the live reloader.
pub fn (mut g FlatGen) set_live_source_path(path string) {
 g.live_source_path = os.real_path(path)
}

fn (g &FlatGen) is_live_function(id flat.NodeId) bool {
 return ('livemain' in g.compile_defines || 'sharedlive' in g.compile_defines)
  && 'live' in g.fn_decl_attributes(id)
}

fn (mut g FlatGen) live_functions() []FlatFnGenItem {
 mut result := []FlatFnGenItem{}
 for item in g.ensure_fn_gen_items() {
  if g.is_live_function(item.node_id) { result << item }
 }
 return result
}

fn (mut g FlatGen) live_function_parameters(node flat.Node) string {
 start := g.sb.len
 g.write_fn_node_params(node)
 return g.sb.cut_to(start)
}

fn (mut g FlatGen) gen_live_declarations() {
 if 'livemain' !in g.compile_defines && 'sharedlive' !in g.compile_defines { return }
 functions := g.live_functions()
 if functions.len == 0 { return }
 if g.target.os == 'windows' {
  g.writeln('static HANDLE __v_live_mutex;\nstatic HANDLE* v_live_fn_mutex_ptr(void) {\n if (g_live_reload_info) { return (HANDLE*)((live__LiveReloadInfo*)g_live_reload_info)->live_fn_mutex; }\n if (!__v_live_mutex) { __v_live_mutex = CreateMutexA(0, 0, 0); }\n return &__v_live_mutex;\n}\nint v_live_mutex_lock(void* mutex) { return WaitForSingleObject(*(HANDLE*)mutex, INFINITE) == WAIT_FAILED; }\nint v_live_mutex_unlock(void* mutex) { return !ReleaseMutex(*(HANDLE*)mutex); }\nstatic void v_live_lock(void) { v_live_mutex_lock(v_live_fn_mutex_ptr()); }\nstatic void v_live_unlock(void) { v_live_mutex_unlock(v_live_fn_mutex_ptr()); }')
 } else {
  g.writeln('#include <pthread.h>\n#include <dlfcn.h>\nstatic pthread_mutex_t __v_live_mutex;\nstatic pthread_once_t __v_live_mutex_once = PTHREAD_ONCE_INIT;\nstatic void v_live_init_mutex(void) {\n pthread_mutexattr_t attr;\n pthread_mutexattr_init(&attr);\n pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);\n pthread_mutex_init(&__v_live_mutex, &attr);\n pthread_mutexattr_destroy(&attr);\n}\nstatic pthread_mutex_t* v_live_fn_mutex_ptr(void) {\n if (g_live_reload_info) { return (pthread_mutex_t*)((live__LiveReloadInfo*)g_live_reload_info)->live_fn_mutex; }\n pthread_once(&__v_live_mutex_once, v_live_init_mutex);\n return &__v_live_mutex;\n}\nstatic void v_live_lock(void) { pthread_mutex_lock(v_live_fn_mutex_ptr()); }\nstatic void v_live_unlock(void) { pthread_mutex_unlock(v_live_fn_mutex_ptr()); }')
 }
 old_module := g.tc.cur_module
 old_file := g.tc.cur_file
 mut bindings := []string{}
 for item in functions {
  g.tc.cur_module = item.module
  g.tc.cur_file = item.file
  node := g.a.nodes[int(item.node_id)]
  name := g.fn_c_name_in_module(item.module, node.value)
  ret := g.fn_return_type_name(g.fn_node_return_type(node, item.module))
  params := g.live_function_parameters(node)
  if 'livemain' in g.compile_defines {
   g.writeln('${ret} no_impl_${name}(${params});')
   g.writeln('static ${ret} (*impl_live_${name})(${params}) = no_impl_${name};')
   lookup := if g.target.os == 'windows' { 'GetProcAddress((HMODULE)live_lib, "impl_live_${name}")' } else { 'dlsym(live_lib, "impl_live_${name}")' }
   bindings << '{ ${ret} (*symbol)(${params}) = (${ret} (*)(${params}))${lookup}; impl_live_${name} = symbol ? symbol : no_impl_${name}; }'
  } else {
   g.writeln('${g.exported_symbol_attribute()}${ret} impl_live_${name}(${params});')
  }
 }
 g.tc.cur_module = old_module
 g.tc.cur_file = old_file
 if 'livemain' in g.compile_defines {
  lookup := if g.target.os == 'windows' { 'GetProcAddress((HMODULE)live_lib, "set_live_reload_pointer")' } else { 'dlsym(live_lib, "set_live_reload_pointer")' }
  g.writeln('static void v_bind_live_symbols(void* live_lib) {')
  for binding in bindings { g.writeln(binding) }
  g.writeln('void (*set_pointer)(void*) = (void (*)(void*))${lookup};')
  g.writeln('if (set_pointer) { set_pointer(g_live_reload_info); }\n}')
 }
}

fn (mut g FlatGen) gen_live_wrapper(node flat.Node, module_name string, name string) {
 ret := g.fn_return_type_name(g.fn_node_return_type(node, module_name))
 params := g.live_function_parameters(node)
 args := g.export_wrapper_arg_names(node).join(', ')
 g.writeln('${ret} ${name}(${params}) {')
 g.writeln('v_live_lock();')
 if ret == 'void' {
  g.writeln('impl_live_${name}(${args});')
  g.writeln('v_live_unlock();')
 } else {
  g.writeln('${ret} __v_live_result = impl_live_${name}(${args});')
  g.writeln('v_live_unlock();')
  g.writeln('return __v_live_result;')
 }
 g.writeln('}')
}

fn (mut g FlatGen) gen_live_initialization() {
 if 'livemain' !in g.compile_defines { return }
 functions := g.live_functions()
 if functions.len == 0 { return }
 options := '-cc ${os.quoted_path(g.ccompiler)} -sharedlive -shared ${if g.is_debug { '-cg' } else { '' }}'
 g.writeln('{')
 g.writeln('live__LiveReloadInfo* live_info = executable__new_live_reload_info(builtin__tos2((u8*)"${reflection_c_string(g.live_source_path)}"), builtin__tos2((u8*)"${reflection_c_string(g.compiler_vexe)}"), builtin__tos2((u8*)"${reflection_c_string(options)}"), v_live_fn_mutex_ptr(), v_bind_live_symbols);')
 mut files := map[string]bool{}
 for item in functions {
  path := os.real_path(item.file)
  if path !in files {
   g.writeln('executable__add_live_monitored_file(live_info, builtin__tos2((u8*)"${reflection_c_string(path)}"));')
   files[path] = true
  }
 }
 g.writeln('g_live_reload_info = live_info;')
 g.writeln('executable__start_reloader(live_info);')
 g.writeln('}')
}
