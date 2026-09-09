// backtrace.h:
#ifndef BACKTRACE_H
#define BACKTRACE_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/* The backtrace state.  This struct is intentionally not defined in
   the public interface.  */

struct backtrace_state;

/* The type of the error callback argument to backtrace functions.
   This function, if not NULL, will be called for certain error cases.
   The DATA argument is passed to the function that calls this one.
   The MSG argument is an error message.  The ERRNUM argument, if
   greater than 0, holds an errno value.  The MSG buffer may become
   invalid after this function returns.

   As a special case, the ERRNUM argument will be passed as -1 if no
   debug info can be found for the executable, or if the debug info
   exists but has an unsupported version, but the function requires
   debug info (e.g., backtrace_full, backtrace_pcinfo).  The MSG in
   this case will be something along the lines of "no debug info".
   Similarly, ERRNUM will be passed as -1 if there is no symbol table,
   but the function requires a symbol table (e.g., backtrace_syminfo).
   This may be used as a signal that some other approach should be
   tried.  */

typedef void (*backtrace_error_callback) (void *data, const char *msg,
					  int errnum);

/* Create state information for the backtrace routines.  This must be
   called before any of the other routines, and its return value must
   be passed to all of the other routines.  FILENAME is the path name
   of the executable file; if it is NULL the library will try
   system-specific path names.  If not NULL, FILENAME must point to a
   permanent buffer.  If THREADED is non-zero the state may be
   accessed by multiple threads simultaneously, and the library will
   use appropriate atomic operations.  If THREADED is zero the state
   may only be accessed by one thread at a time.  This returns a state
   pointer on success, NULL on error.  If an error occurs, this will
   call the ERROR_CALLBACK routine.

   Calling this function allocates resources that cannot be freed.
   There is no backtrace_free_state function.  The state is used to
   cache information that is expensive to recompute.  Programs are
   expected to call this function at most once and to save the return
   value for all later calls to backtrace functions.  */

extern struct backtrace_state *backtrace_create_state (
    const char *filename, int threaded,
    backtrace_error_callback error_callback, void *data);

/* The type of the callback argument to the backtrace_full function.
   DATA is the argument passed to backtrace_full.  PC is the program
   counter.  FILENAME is the name of the file containing PC, or NULL
   if not available.  LINENO is the line number in FILENAME containing
   PC, or 0 if not available.  FUNCTION is the name of the function
   containing PC, or NULL if not available.  This should return 0 to
   continuing tracing.  The FILENAME and FUNCTION buffers may become
   invalid after this function returns.  */

typedef int (*backtrace_full_callback) (void *data, uintptr_t pc,
					const char *filename, int lineno,
					const char *function);

/* Get a full stack backtrace.  SKIP is the number of frames to skip;
   passing 0 will start the trace with the function calling
   backtrace_full.  DATA is passed to the callback routine.  If any
   call to CALLBACK returns a non-zero value, the stack backtrace
   stops, and backtrace returns that value; this may be used to limit
   the number of stack frames desired.  If all calls to CALLBACK
   return 0, backtrace returns 0.  The backtrace_full function will
   make at least one call to either CALLBACK or ERROR_CALLBACK.  This
   function requires debug info for the executable.  */

extern int backtrace_full (struct backtrace_state *state, int skip,
			   backtrace_full_callback callback,
			   backtrace_error_callback error_callback,
			   void *data);

/* The type of the callback argument to the backtrace_simple function.
   DATA is the argument passed to simple_backtrace.  PC is the program
   counter.  This should return 0 to continue tracing.  */

typedef int (*backtrace_simple_callback) (void *data, uintptr_t pc);

/* Get a simple backtrace.  SKIP is the number of frames to skip, as
   in backtrace.  DATA is passed to the callback routine.  If any call
   to CALLBACK returns a non-zero value, the stack backtrace stops,
   and backtrace_simple returns that value.  Otherwise
   backtrace_simple returns 0.  The backtrace_simple function will
   make at least one call to either CALLBACK or ERROR_CALLBACK.  This
   function does not require any debug info for the executable.  */

extern int backtrace_simple (struct backtrace_state *state, int skip,
			     backtrace_simple_callback callback,
			     backtrace_error_callback error_callback,
			     void *data);

/* Print the current backtrace in a user readable format to a FILE.
   SKIP is the number of frames to skip, as in backtrace_full.  Any
   error messages are printed to stderr.  This function requires debug
   info for the executable.  */

extern void backtrace_print (struct backtrace_state *state, int skip, FILE *);

/* Given PC, a program counter in the current program, call the
   callback function with filename, line number, and function name
   information.  This will normally call the callback function exactly
   once.  However, if the PC happens to describe an inlined call, and
   the debugging information contains the necessary information, then
   this may call the callback function multiple times.  This will make
   at least one call to either CALLBACK or ERROR_CALLBACK.  This
   returns the first non-zero value returned by CALLBACK, or 0.  */

extern int backtrace_pcinfo (struct backtrace_state *state, uintptr_t pc,
			     backtrace_full_callback callback,
			     backtrace_error_callback error_callback,
			     void *data);

/* The type of the callback argument to backtrace_syminfo.  DATA and
   PC are the arguments passed to backtrace_syminfo.  SYMNAME is the
   name of the symbol for the corresponding code.  SYMVAL is the
   value and SYMSIZE is the size of the symbol.  SYMNAME will be NULL
   if no error occurred but the symbol could not be found.  */

typedef void (*backtrace_syminfo_callback) (void *data, uintptr_t pc,
					    const char *symname,
					    uintptr_t symval,
					    uintptr_t symsize);

/* Given ADDR, an address or program counter in the current program,
   call the callback information with the symbol name and value
   describing the function or variable in which ADDR may be found.
   This will call either CALLBACK or ERROR_CALLBACK exactly once.
   This returns 1 on success, 0 on failure.  This function requires
   the symbol table but does not require the debug info.  Note that if
   the symbol table is present but ADDR could not be found in the
   table, CALLBACK will be called with a NULL SYMNAME argument.
   Returns 1 on success, 0 on error.  */

extern int backtrace_syminfo (struct backtrace_state *state, uintptr_t addr,
			      backtrace_syminfo_callback callback,
			      backtrace_error_callback error_callback,
			      void *data);

#ifdef __cplusplus
} /* End extern "C".  */
#endif

#endif
// internal.h:
#ifndef BACKTRACE_INTERNAL_H
#define BACKTRACE_INTERNAL_H

/* We assume that <sys/types.h> and "backtrace.h" have already been
   included.  */

#ifndef GCC_VERSION
# define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif

#if (GCC_VERSION < 2007)
# define __attribute__(x)
#endif

#ifndef ATTRIBUTE_UNUSED
# define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif

#ifndef ATTRIBUTE_MALLOC
# if (GCC_VERSION >= 2096)
#  define ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
# else
#  define ATTRIBUTE_MALLOC
# endif
#endif

#ifdef __has_attribute
# if __has_attribute(fallthrough)
#  define ATTRIBUTE_FALLTHROUGH __attribute__ ((fallthrough))
# endif
#endif
#ifndef ATTRIBUTE_FALLTHROUGH
# if (GCC_VERSION >= 7000)
#  define ATTRIBUTE_FALLTHROUGH __attribute__ ((__fallthrough__))
# else
#  define ATTRIBUTE_FALLTHROUGH
# endif
#endif

#ifndef HAVE_SYNC_FUNCTIONS

/* Define out the sync functions.  These should never be called if
   they are not available.  */

#define __sync_bool_compare_and_swap(A, B, C) (abort(), 1)
#define __sync_lock_test_and_set(A, B) (abort(), 0)
#define __sync_lock_release(A) abort()

#endif /* !defined (HAVE_SYNC_FUNCTIONS) */

#ifdef HAVE_ATOMIC_FUNCTIONS

/* We have the atomic builtin functions.  */

#define backtrace_atomic_load_pointer(p) \
    __atomic_load_n ((p), __ATOMIC_ACQUIRE)
#define backtrace_atomic_load_int(p) \
    __atomic_load_n ((p), __ATOMIC_ACQUIRE)
#define backtrace_atomic_store_pointer(p, v) \
    __atomic_store_n ((p), (v), __ATOMIC_RELEASE)
#define backtrace_atomic_store_size_t(p, v) \
    __atomic_store_n ((p), (v), __ATOMIC_RELEASE)
#define backtrace_atomic_store_int(p, v) \
    __atomic_store_n ((p), (v), __ATOMIC_RELEASE)

#else /* !defined (HAVE_ATOMIC_FUNCTIONS) */
#ifdef HAVE_SYNC_FUNCTIONS

/* We have the sync functions but not the atomic functions.  Define
   the atomic ones in terms of the sync ones.  */

extern void *backtrace_atomic_load_pointer (void *);
extern int backtrace_atomic_load_int (int *);
extern void backtrace_atomic_store_pointer (void *, void *);
extern void backtrace_atomic_store_size_t (size_t *, size_t);
extern void backtrace_atomic_store_int (int *, int);

#else /* !defined (HAVE_SYNC_FUNCTIONS) */

/* We have neither the sync nor the atomic functions.  These will
   never be called.  */

#define backtrace_atomic_load_pointer(p) (abort(), (void *) NULL)
#define backtrace_atomic_load_int(p) (abort(), 0)
#define backtrace_atomic_store_pointer(p, v) abort()
#define backtrace_atomic_store_size_t(p, v) abort()
#define backtrace_atomic_store_int(p, v) abort()

#endif /* !defined (HAVE_SYNC_FUNCTIONS) */
#endif /* !defined (HAVE_ATOMIC_FUNCTIONS) */

/* The type of the function that collects file/line information.  This
   is like backtrace_pcinfo.  */

typedef int (*fileline) (struct backtrace_state *state, uintptr_t pc,
			 backtrace_full_callback callback,
			 backtrace_error_callback error_callback, void *data);

/* The type of the function that collects symbol information.  This is
   like backtrace_syminfo.  */

typedef void (*syminfo) (struct backtrace_state *state, uintptr_t pc,
			 backtrace_syminfo_callback callback,
			 backtrace_error_callback error_callback, void *data);

/* What the backtrace state pointer points to.  */

struct backtrace_state
{
  /* The name of the executable.  */
  const char *filename;
  /* Non-zero if threaded.  */
  int threaded;
  /* The master lock for fileline_fn, fileline_data, syminfo_fn,
     syminfo_data, fileline_initialization_failed and everything the
     data pointers point to.  */
  void *lock;
  /* The function that returns file/line information.  */
  fileline fileline_fn;
  /* The data to pass to FILELINE_FN.  */
  void *fileline_data;
  /* The function that returns symbol information.  */
  syminfo syminfo_fn;
  /* The data to pass to SYMINFO_FN.  */
  void *syminfo_data;
  /* Whether initializing the file/line information failed.  */
  int fileline_initialization_failed;
  /* The lock for the freelist.  */
  int lock_alloc;
  /* The freelist when using mmap.  */
  struct backtrace_freelist_struct *freelist;
};

/* Open a file for reading.  Returns -1 on error.  If DOES_NOT_EXIST
   is not NULL, *DOES_NOT_EXIST will be set to 0 normally and set to 1
   if the file does not exist.  If the file does not exist and
   DOES_NOT_EXIST is not NULL, the function will return -1 and will
   not call ERROR_CALLBACK.  On other errors, or if DOES_NOT_EXIST is
   NULL, the function will call ERROR_CALLBACK before returning.  */
extern int backtrace_open (const char *filename,
			   backtrace_error_callback error_callback,
			   void *data,
			   int *does_not_exist);

/* A view of the contents of a file.  This supports mmap when
   available.  A view will remain in memory even after backtrace_close
   is called on the file descriptor from which the view was
   obtained.  */

struct backtrace_view
{
  /* The data that the caller requested.  */
  const void *data;
  /* The base of the view.  */
  void *base;
  /* The total length of the view.  */
  size_t len;
};

/* Create a view of SIZE bytes from DESCRIPTOR at OFFSET.  Store the
   result in *VIEW.  Returns 1 on success, 0 on error.  */
extern int backtrace_get_view (struct backtrace_state *state, int descriptor,
			       off_t offset, uint64_t size,
			       backtrace_error_callback error_callback,
			       void *data, struct backtrace_view *view);

/* Release a view created by backtrace_get_view.  */
extern void backtrace_release_view (struct backtrace_state *state,
				    struct backtrace_view *view,
				    backtrace_error_callback error_callback,
				    void *data);

/* Close a file opened by backtrace_open.  Returns 1 on success, 0 on
   error.  */

extern int backtrace_close (int descriptor,
			    backtrace_error_callback error_callback,
			    void *data);

/* Sort without using memory.  */

extern void backtrace_qsort (void *base, size_t count, size_t size,
			     int (*compar) (const void *, const void *));

/* Allocate memory.  This is like malloc.  If ERROR_CALLBACK is NULL,
   this does not report an error, it just returns NULL.  */

extern void *backtrace_alloc (struct backtrace_state *state, size_t size,
			      backtrace_error_callback error_callback,
			      void *data) ATTRIBUTE_MALLOC;

/* Free memory allocated by backtrace_alloc.  If ERROR_CALLBACK is
   NULL, this does not report an error.  */

extern void backtrace_free (struct backtrace_state *state, void *mem,
			    size_t size,
			    backtrace_error_callback error_callback,
			    void *data);

/* A growable vector of some struct.  This is used for more efficient
   allocation when we don't know the final size of some group of data
   that we want to represent as an array.  */

struct backtrace_vector
{
  /* The base of the vector.  */
  void *base;
  /* The number of bytes in the vector.  */
  size_t size;
  /* The number of bytes available at the current allocation.  */
  size_t alc;
};

/* Grow VEC by SIZE bytes.  Return a pointer to the newly allocated
   bytes.  Note that this may move the entire vector to a new memory
   location.  Returns NULL on failure.  */

extern void *backtrace_vector_grow (struct backtrace_state *state, size_t size,
				    backtrace_error_callback error_callback,
				    void *data,
				    struct backtrace_vector *vec);

/* Finish the current allocation on VEC.  Prepare to start a new
   allocation.  The finished allocation will never be freed.  Returns
   a pointer to the base of the finished entries, or NULL on
   failure.  */

extern void* backtrace_vector_finish (struct backtrace_state *state,
				      struct backtrace_vector *vec,
				      backtrace_error_callback error_callback,
				      void *data);

/* Release any extra space allocated for VEC.  This may change
   VEC->base.  Returns 1 on success, 0 on failure.  */

extern int backtrace_vector_release (struct backtrace_state *state,
				     struct backtrace_vector *vec,
				     backtrace_error_callback error_callback,
				     void *data);

/* Free the space managed by VEC.  This will reset VEC.  */

static inline void
backtrace_vector_free (struct backtrace_state *state,
		       struct backtrace_vector *vec,
		       backtrace_error_callback error_callback, void *data)
{
  vec->alc += vec->size;
  vec->size = 0;
  backtrace_vector_release (state, vec, error_callback, data);
}

/* Read initial debug data from a descriptor, and set the
   fileline_data, syminfo_fn, and syminfo_data fields of STATE.
   Return the fileln_fn field in *FILELN_FN--this is done this way so
   that the synchronization code is only implemented once.  This is
   called after the descriptor has first been opened.  It will close
   the descriptor if it is no longer needed.  Returns 1 on success, 0
   on error.  There will be multiple implementations of this function,
   for different file formats.  Each system will compile the
   appropriate one.  */

extern int backtrace_initialize (struct backtrace_state *state,
				 const char *filename,
				 int descriptor,
				 backtrace_error_callback error_callback,
				 void *data,
				 fileline *fileline_fn);

/* An enum for the DWARF sections we care about.  */

enum dwarf_section
{
  DEBUG_INFO,
  DEBUG_LINE,
  DEBUG_ABBREV,
  DEBUG_RANGES,
  DEBUG_STR,
  DEBUG_ADDR,
  DEBUG_STR_OFFSETS,
  DEBUG_LINE_STR,
  DEBUG_RNGLISTS,

  DEBUG_MAX
};

/* Data for the DWARF sections we care about.  */

struct dwarf_sections
{
  const unsigned char *data[DEBUG_MAX];
  size_t size[DEBUG_MAX];
};

/* DWARF data read from a file, used for .gnu_debugaltlink.  */

struct dwarf_data;

/* The load address mapping.  */

#if defined(__FDPIC__) && defined(HAVE_DL_ITERATE_PHDR) && (defined(HAVE_LINK_H) || defined(HAVE_SYS_LINK_H))

#ifdef HAVE_LINK_H
 #include <link.h>
#endif
#ifdef HAVE_SYS_LINK_H
 #include <sys/link.h>
#endif

#define libbacktrace_using_fdpic() (1)

struct libbacktrace_base_address
{
  struct elf32_fdpic_loadaddr m;
};

#define libbacktrace_add_base(pc, base) \
  ((uintptr_t) (__RELOC_POINTER ((pc), (base).m)))

#else /* not _FDPIC__ */

#define libbacktrace_using_fdpic() (0)

struct libbacktrace_base_address
{
  uintptr_t m;
};

#define libbacktrace_add_base(pc, base) ((pc) + (base).m)

#endif /* not _FDPIC__ */

/* Add file/line information for a DWARF module.  */

extern int backtrace_dwarf_add (struct backtrace_state *state,
				struct libbacktrace_base_address base_address,
				const struct dwarf_sections *dwarf_sections,
				int is_bigendian,
				struct dwarf_data *fileline_altlink,
				backtrace_error_callback error_callback,
				void *data, fileline *fileline_fn,
				struct dwarf_data **fileline_entry);

/* A data structure to pass to backtrace_syminfo_to_full.  */

struct backtrace_call_full
{
  backtrace_full_callback full_callback;
  backtrace_error_callback full_error_callback;
  void *full_data;
  int ret;
};

/* A backtrace_syminfo_callback that can call into a
   backtrace_full_callback, used when we have a symbol table but no
   debug info.  */

extern void backtrace_syminfo_to_full_callback (void *data, uintptr_t pc,
						const char *symname,
						uintptr_t symval,
						uintptr_t symsize);

/* An error callback that corresponds to
   backtrace_syminfo_to_full_callback.  */

extern void backtrace_syminfo_to_full_error_callback (void *, const char *,
						      int);

/* A test-only hook for elf_uncompress_zdebug.  */

extern int backtrace_uncompress_zdebug (struct backtrace_state *,
					const unsigned char *compressed,
					size_t compressed_size,
					backtrace_error_callback, void *data,
					unsigned char **uncompressed,
					size_t *uncompressed_size);

/* A test-only hook for elf_zstd_decompress.  */

extern int backtrace_uncompress_zstd (struct backtrace_state *,
				      const unsigned char *compressed,
				      size_t compressed_size,
				      backtrace_error_callback, void *data,
				      unsigned char *uncompressed,
				      size_t uncompressed_size);

/* A test-only hook for elf_uncompress_lzma.  */

extern int backtrace_uncompress_lzma (struct backtrace_state *,
				      const unsigned char *compressed,
				      size_t compressed_size,
				      backtrace_error_callback, void *data,
				      unsigned char **uncompressed,
				      size_t *uncompressed_size);

#endif
// filenames.h:
#ifndef GCC_VERSION
# define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif

#if (GCC_VERSION < 2007)
# define __attribute__(x)
#endif

#ifndef ATTRIBUTE_UNUSED
# define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif

#if defined(__MSDOS__) || defined(_WIN32) || defined(__OS2__) || defined (__CYGWIN__)
# define IS_DIR_SEPARATOR(c) ((c) == '/' || (c) == '\\')
# define HAS_DRIVE_SPEC(f) ((f)[0] != '\0' && (f)[1] == ':')
# define IS_ABSOLUTE_PATH(f) (IS_DIR_SEPARATOR((f)[0]) || HAS_DRIVE_SPEC(f))
#else
# define IS_DIR_SEPARATOR(c) ((c) == '/')
# define IS_ABSOLUTE_PATH(f) (IS_DIR_SEPARATOR((f)[0]))
#endif
// atomic.c:
#include <sys/types.h>


/* This file holds implementations of the atomic functions that are
   used if the host compiler has the sync functions but not the atomic
   functions, as is true of versions of GCC before 4.7.  */

#if !defined (HAVE_ATOMIC_FUNCTIONS) && defined (HAVE_SYNC_FUNCTIONS)

/* Do an atomic load of a pointer.  */

void *
backtrace_atomic_load_pointer (void *arg)
{
  void **pp;
  void *p;

  pp = (void **) arg;
  p = *pp;
  while (!__sync_bool_compare_and_swap (pp, p, p))
    p = *pp;
  return p;
}

/* Do an atomic load of an int.  */

int
backtrace_atomic_load_int (int *p)
{
  int i;

  i = *p;
  while (!__sync_bool_compare_and_swap (p, i, i))
    i = *p;
  return i;
}

/* Do an atomic store of a pointer.  */

void
backtrace_atomic_store_pointer (void *arg, void *p)
{
  void **pp;
  void *old;

  pp = (void **) arg;
  old = *pp;
  while (!__sync_bool_compare_and_swap (pp, old, p))
    old = *pp;
}

/* Do an atomic store of a size_t value.  */

void
backtrace_atomic_store_size_t (size_t *p, size_t v)
{
  size_t old;

  old = *p;
  while (!__sync_bool_compare_and_swap (p, old, v))
    old = *p;
}

/* Do an atomic store of a int value.  */

void
backtrace_atomic_store_int (int *p, int v)
{
  int old;

  old = *p;
  while (!__sync_bool_compare_and_swap (p, old, v))
    old = *p;
}

#endif
// dwarf.c:
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>



/* DWARF constants.  */

enum dwarf_tag {
  DW_TAG_entry_point = 0x3,
  DW_TAG_compile_unit = 0x11,
  DW_TAG_inlined_subroutine = 0x1d,
  DW_TAG_subprogram = 0x2e,
  DW_TAG_skeleton_unit = 0x4a,
};

enum dwarf_form {
  DW_FORM_addr = 0x01,
  DW_FORM_block2 = 0x03,
  DW_FORM_block4 = 0x04,
  DW_FORM_data2 = 0x05,
  DW_FORM_data4 = 0x06,
  DW_FORM_data8 = 0x07,
  DW_FORM_string = 0x08,
  DW_FORM_block = 0x09,
  DW_FORM_block1 = 0x0a,
  DW_FORM_data1 = 0x0b,
  DW_FORM_flag = 0x0c,
  DW_FORM_sdata = 0x0d,
  DW_FORM_strp = 0x0e,
  DW_FORM_udata = 0x0f,
  DW_FORM_ref_addr = 0x10,
  DW_FORM_ref1 = 0x11,
  DW_FORM_ref2 = 0x12,
  DW_FORM_ref4 = 0x13,
  DW_FORM_ref8 = 0x14,
  DW_FORM_ref_udata = 0x15,
  DW_FORM_indirect = 0x16,
  DW_FORM_sec_offset = 0x17,
  DW_FORM_exprloc = 0x18,
  DW_FORM_flag_present = 0x19,
  DW_FORM_ref_sig8 = 0x20,
  DW_FORM_strx = 0x1a,
  DW_FORM_addrx = 0x1b,
  DW_FORM_ref_sup4 = 0x1c,
  DW_FORM_strp_sup = 0x1d,
  DW_FORM_data16 = 0x1e,
  DW_FORM_line_strp = 0x1f,
  DW_FORM_implicit_const = 0x21,
  DW_FORM_loclistx = 0x22,
  DW_FORM_rnglistx = 0x23,
  DW_FORM_ref_sup8 = 0x24,
  DW_FORM_strx1 = 0x25,
  DW_FORM_strx2 = 0x26,
  DW_FORM_strx3 = 0x27,
  DW_FORM_strx4 = 0x28,
  DW_FORM_addrx1 = 0x29,
  DW_FORM_addrx2 = 0x2a,
  DW_FORM_addrx3 = 0x2b,
  DW_FORM_addrx4 = 0x2c,
  DW_FORM_GNU_addr_index = 0x1f01,
  DW_FORM_GNU_str_index = 0x1f02,
  DW_FORM_GNU_ref_alt = 0x1f20,
  DW_FORM_GNU_strp_alt = 0x1f21
};

enum dwarf_attribute {
  DW_AT_sibling = 0x01,
  DW_AT_location = 0x02,
  DW_AT_name = 0x03,
  DW_AT_ordering = 0x09,
  DW_AT_subscr_data = 0x0a,
  DW_AT_byte_size = 0x0b,
  DW_AT_bit_offset = 0x0c,
  DW_AT_bit_size = 0x0d,
  DW_AT_element_list = 0x0f,
  DW_AT_stmt_list = 0x10,
  DW_AT_low_pc = 0x11,
  DW_AT_high_pc = 0x12,
  DW_AT_language = 0x13,
  DW_AT_member = 0x14,
  DW_AT_discr = 0x15,
  DW_AT_discr_value = 0x16,
  DW_AT_visibility = 0x17,
  DW_AT_import = 0x18,
  DW_AT_string_length = 0x19,
  DW_AT_common_reference = 0x1a,
  DW_AT_comp_dir = 0x1b,
  DW_AT_const_value = 0x1c,
  DW_AT_containing_type = 0x1d,
  DW_AT_default_value = 0x1e,
  DW_AT_inline = 0x20,
  DW_AT_is_optional = 0x21,
  DW_AT_lower_bound = 0x22,
  DW_AT_producer = 0x25,
  DW_AT_prototyped = 0x27,
  DW_AT_return_addr = 0x2a,
  DW_AT_start_scope = 0x2c,
  DW_AT_bit_stride = 0x2e,
  DW_AT_upper_bound = 0x2f,
  DW_AT_abstract_origin = 0x31,
  DW_AT_accessibility = 0x32,
  DW_AT_address_class = 0x33,
  DW_AT_artificial = 0x34,
  DW_AT_base_types = 0x35,
  DW_AT_calling_convention = 0x36,
  DW_AT_count = 0x37,
  DW_AT_data_member_location = 0x38,
  DW_AT_decl_column = 0x39,
  DW_AT_decl_file = 0x3a,
  DW_AT_decl_line = 0x3b,
  DW_AT_declaration = 0x3c,
  DW_AT_discr_list = 0x3d,
  DW_AT_encoding = 0x3e,
  DW_AT_external = 0x3f,
  DW_AT_frame_base = 0x40,
  DW_AT_friend = 0x41,
  DW_AT_identifier_case = 0x42,
  DW_AT_macro_info = 0x43,
  DW_AT_namelist_items = 0x44,
  DW_AT_priority = 0x45,
  DW_AT_segment = 0x46,
  DW_AT_specification = 0x47,
  DW_AT_static_link = 0x48,
  DW_AT_type = 0x49,
  DW_AT_use_location = 0x4a,
  DW_AT_variable_parameter = 0x4b,
  DW_AT_virtuality = 0x4c,
  DW_AT_vtable_elem_location = 0x4d,
  DW_AT_allocated = 0x4e,
  DW_AT_associated = 0x4f,
  DW_AT_data_location = 0x50,
  DW_AT_byte_stride = 0x51,
  DW_AT_entry_pc = 0x52,
  DW_AT_use_UTF8 = 0x53,
  DW_AT_extension = 0x54,
  DW_AT_ranges = 0x55,
  DW_AT_trampoline = 0x56,
  DW_AT_call_column = 0x57,
  DW_AT_call_file = 0x58,
  DW_AT_call_line = 0x59,
  DW_AT_description = 0x5a,
  DW_AT_binary_scale = 0x5b,
  DW_AT_decimal_scale = 0x5c,
  DW_AT_small = 0x5d,
  DW_AT_decimal_sign = 0x5e,
  DW_AT_digit_count = 0x5f,
  DW_AT_picture_string = 0x60,
  DW_AT_mutable = 0x61,
  DW_AT_threads_scaled = 0x62,
  DW_AT_explicit = 0x63,
  DW_AT_object_pointer = 0x64,
  DW_AT_endianity = 0x65,
  DW_AT_elemental = 0x66,
  DW_AT_pure = 0x67,
  DW_AT_recursive = 0x68,
  DW_AT_signature = 0x69,
  DW_AT_main_subprogram = 0x6a,
  DW_AT_data_bit_offset = 0x6b,
  DW_AT_const_expr = 0x6c,
  DW_AT_enum_class = 0x6d,
  DW_AT_linkage_name = 0x6e,
  DW_AT_string_length_bit_size = 0x6f,
  DW_AT_string_length_byte_size = 0x70,
  DW_AT_rank = 0x71,
  DW_AT_str_offsets_base = 0x72,
  DW_AT_addr_base = 0x73,
  DW_AT_rnglists_base = 0x74,
  DW_AT_dwo_name = 0x76,
  DW_AT_reference = 0x77,
  DW_AT_rvalue_reference = 0x78,
  DW_AT_macros = 0x79,
  DW_AT_call_all_calls = 0x7a,
  DW_AT_call_all_source_calls = 0x7b,
  DW_AT_call_all_tail_calls = 0x7c,
  DW_AT_call_return_pc = 0x7d,
  DW_AT_call_value = 0x7e,
  DW_AT_call_origin = 0x7f,
  DW_AT_call_parameter = 0x80,
  DW_AT_call_pc = 0x81,
  DW_AT_call_tail_call = 0x82,
  DW_AT_call_target = 0x83,
  DW_AT_call_target_clobbered = 0x84,
  DW_AT_call_data_location = 0x85,
  DW_AT_call_data_value = 0x86,
  DW_AT_noreturn = 0x87,
  DW_AT_alignment = 0x88,
  DW_AT_export_symbols = 0x89,
  DW_AT_deleted = 0x8a,
  DW_AT_defaulted = 0x8b,
  DW_AT_loclists_base = 0x8c,
  DW_AT_lo_user = 0x2000,
  DW_AT_hi_user = 0x3fff,
  DW_AT_MIPS_fde = 0x2001,
  DW_AT_MIPS_loop_begin = 0x2002,
  DW_AT_MIPS_tail_loop_begin = 0x2003,
  DW_AT_MIPS_epilog_begin = 0x2004,
  DW_AT_MIPS_loop_unroll_factor = 0x2005,
  DW_AT_MIPS_software_pipeline_depth = 0x2006,
  DW_AT_MIPS_linkage_name = 0x2007,
  DW_AT_MIPS_stride = 0x2008,
  DW_AT_MIPS_abstract_name = 0x2009,
  DW_AT_MIPS_clone_origin = 0x200a,
  DW_AT_MIPS_has_inlines = 0x200b,
  DW_AT_HP_block_index = 0x2000,
  DW_AT_HP_unmodifiable = 0x2001,
  DW_AT_HP_prologue = 0x2005,
  DW_AT_HP_epilogue = 0x2008,
  DW_AT_HP_actuals_stmt_list = 0x2010,
  DW_AT_HP_proc_per_section = 0x2011,
  DW_AT_HP_raw_data_ptr = 0x2012,
  DW_AT_HP_pass_by_reference = 0x2013,
  DW_AT_HP_opt_level = 0x2014,
  DW_AT_HP_prof_version_id = 0x2015,
  DW_AT_HP_opt_flags = 0x2016,
  DW_AT_HP_cold_region_low_pc = 0x2017,
  DW_AT_HP_cold_region_high_pc = 0x2018,
  DW_AT_HP_all_variables_modifiable = 0x2019,
  DW_AT_HP_linkage_name = 0x201a,
  DW_AT_HP_prof_flags = 0x201b,
  DW_AT_HP_unit_name = 0x201f,
  DW_AT_HP_unit_size = 0x2020,
  DW_AT_HP_widened_byte_size = 0x2021,
  DW_AT_HP_definition_points = 0x2022,
  DW_AT_HP_default_location = 0x2023,
  DW_AT_HP_is_result_param = 0x2029,
  DW_AT_sf_names = 0x2101,
  DW_AT_src_info = 0x2102,
  DW_AT_mac_info = 0x2103,
  DW_AT_src_coords = 0x2104,
  DW_AT_body_begin = 0x2105,
  DW_AT_body_end = 0x2106,
  DW_AT_GNU_vector = 0x2107,
  DW_AT_GNU_guarded_by = 0x2108,
  DW_AT_GNU_pt_guarded_by = 0x2109,
  DW_AT_GNU_guarded = 0x210a,
  DW_AT_GNU_pt_guarded = 0x210b,
  DW_AT_GNU_locks_excluded = 0x210c,
  DW_AT_GNU_exclusive_locks_required = 0x210d,
  DW_AT_GNU_shared_locks_required = 0x210e,
  DW_AT_GNU_odr_signature = 0x210f,
  DW_AT_GNU_template_name = 0x2110,
  DW_AT_GNU_call_site_value = 0x2111,
  DW_AT_GNU_call_site_data_value = 0x2112,
  DW_AT_GNU_call_site_target = 0x2113,
  DW_AT_GNU_call_site_target_clobbered = 0x2114,
  DW_AT_GNU_tail_call = 0x2115,
  DW_AT_GNU_all_tail_call_sites = 0x2116,
  DW_AT_GNU_all_call_sites = 0x2117,
  DW_AT_GNU_all_source_call_sites = 0x2118,
  DW_AT_GNU_macros = 0x2119,
  DW_AT_GNU_deleted = 0x211a,
  DW_AT_GNU_dwo_name = 0x2130,
  DW_AT_GNU_dwo_id = 0x2131,
  DW_AT_GNU_ranges_base = 0x2132,
  DW_AT_GNU_addr_base = 0x2133,
  DW_AT_GNU_pubnames = 0x2134,
  DW_AT_GNU_pubtypes = 0x2135,
  DW_AT_GNU_discriminator = 0x2136,
  DW_AT_GNU_locviews = 0x2137,
  DW_AT_GNU_entry_view = 0x2138,
  DW_AT_VMS_rtnbeg_pd_address = 0x2201,
  DW_AT_use_GNAT_descriptive_type = 0x2301,
  DW_AT_GNAT_descriptive_type = 0x2302,
  DW_AT_GNU_numerator = 0x2303,
  DW_AT_GNU_denominator = 0x2304,
  DW_AT_GNU_bias = 0x2305,
  DW_AT_upc_threads_scaled = 0x3210,
  DW_AT_PGI_lbase = 0x3a00,
  DW_AT_PGI_soffset = 0x3a01,
  DW_AT_PGI_lstride = 0x3a02,
  DW_AT_APPLE_optimized = 0x3fe1,
  DW_AT_APPLE_flags = 0x3fe2,
  DW_AT_APPLE_isa = 0x3fe3,
  DW_AT_APPLE_block = 0x3fe4,
  DW_AT_APPLE_major_runtime_vers = 0x3fe5,
  DW_AT_APPLE_runtime_class = 0x3fe6,
  DW_AT_APPLE_omit_frame_ptr = 0x3fe7,
  DW_AT_APPLE_property_name = 0x3fe8,
  DW_AT_APPLE_property_getter = 0x3fe9,
  DW_AT_APPLE_property_setter = 0x3fea,
  DW_AT_APPLE_property_attribute = 0x3feb,
  DW_AT_APPLE_objc_complete_type = 0x3fec,
  DW_AT_APPLE_property = 0x3fed
};

enum dwarf_line_number_op {
  DW_LNS_extended_op = 0x0,
  DW_LNS_copy = 0x1,
  DW_LNS_advance_pc = 0x2,
  DW_LNS_advance_line = 0x3,
  DW_LNS_set_file = 0x4,
  DW_LNS_set_column = 0x5,
  DW_LNS_negate_stmt = 0x6,
  DW_LNS_set_basic_block = 0x7,
  DW_LNS_const_add_pc = 0x8,
  DW_LNS_fixed_advance_pc = 0x9,
  DW_LNS_set_prologue_end = 0xa,
  DW_LNS_set_epilogue_begin = 0xb,
  DW_LNS_set_isa = 0xc,
};

enum dwarf_extended_line_number_op {
  DW_LNE_end_sequence = 0x1,
  DW_LNE_set_address = 0x2,
  DW_LNE_define_file = 0x3,
  DW_LNE_set_discriminator = 0x4,
};

enum dwarf_line_number_content_type {
  DW_LNCT_path = 0x1,
  DW_LNCT_directory_index = 0x2,
  DW_LNCT_timestamp = 0x3,
  DW_LNCT_size = 0x4,
  DW_LNCT_MD5 = 0x5,
  DW_LNCT_lo_user = 0x2000,
  DW_LNCT_hi_user = 0x3fff
};

enum dwarf_range_list_entry {
  DW_RLE_end_of_list = 0x00,
  DW_RLE_base_addressx = 0x01,
  DW_RLE_startx_endx = 0x02,
  DW_RLE_startx_length = 0x03,
  DW_RLE_offset_pair = 0x04,
  DW_RLE_base_address = 0x05,
  DW_RLE_start_end = 0x06,
  DW_RLE_start_length = 0x07
};

enum dwarf_unit_type {
  DW_UT_compile = 0x01,
  DW_UT_type = 0x02,
  DW_UT_partial = 0x03,
  DW_UT_skeleton = 0x04,
  DW_UT_split_compile = 0x05,
  DW_UT_split_type = 0x06,
  DW_UT_lo_user = 0x80,
  DW_UT_hi_user = 0xff
};

#if !defined(HAVE_DECL_STRNLEN) || !HAVE_DECL_STRNLEN

/* If strnlen is not declared, provide our own version.  */

static size_t
xstrnlen (const char *s, size_t maxlen)
{
  size_t i;

  for (i = 0; i < maxlen; ++i)
    if (s[i] == '\0')
      break;
  return i;
}

#define strnlen xstrnlen

#endif

/* A buffer to read DWARF info.  */

struct dwarf_buf
{
  /* Buffer name for error messages.  */
  const char *name;
  /* Start of the buffer.  */
  const unsigned char *start;
  /* Next byte to read.  */
  const unsigned char *buf;
  /* The number of bytes remaining.  */
  size_t left;
  /* Whether the data is big-endian.  */
  int is_bigendian;
  /* Error callback routine.  */
  backtrace_error_callback error_callback;
  /* Data for error_callback.  */
  void *data;
  /* Non-zero if we've reported an underflow error.  */
  int reported_underflow;
};

/* A single attribute in a DWARF abbreviation.  */

struct attr
{
  /* The attribute name.  */
  enum dwarf_attribute name;
  /* The attribute form.  */
  enum dwarf_form form;
  /* The attribute value, for DW_FORM_implicit_const.  */
  int64_t val;
};

/* A single DWARF abbreviation.  */

struct abbrev
{
  /* The abbrev code--the number used to refer to the abbrev.  */
  uint64_t code;
  /* The entry tag.  */
  enum dwarf_tag tag;
  /* Non-zero if this abbrev has child entries.  */
  int has_children;
  /* The number of attributes.  */
  size_t num_attrs;
  /* The attributes.  */
  struct attr *attrs;
};

/* The DWARF abbreviations for a compilation unit.  This structure
   only exists while reading the compilation unit.  Most DWARF readers
   seem to a hash table to map abbrev ID's to abbrev entries.
   However, we primarily care about GCC, and GCC simply issues ID's in
   numerical order starting at 1.  So we simply keep a sorted vector,
   and try to just look up the code.  */

struct abbrevs
{
  /* The number of abbrevs in the vector.  */
  size_t num_abbrevs;
  /* The abbrevs, sorted by the code field.  */
  struct abbrev *abbrevs;
};

/* The different kinds of attribute values.  */

enum attr_val_encoding
{
  /* No attribute value.  */
  ATTR_VAL_NONE,
  /* An address.  */
  ATTR_VAL_ADDRESS,
  /* An index into the .debug_addr section, whose value is relative to
     the DW_AT_addr_base attribute of the compilation unit.  */
  ATTR_VAL_ADDRESS_INDEX,
  /* A unsigned integer.  */
  ATTR_VAL_UINT,
  /* A sigd integer.  */
  ATTR_VAL_SINT,
  /* A string.  */
  ATTR_VAL_STRING,
  /* An index into the .debug_str_offsets section.  */
  ATTR_VAL_STRING_INDEX,
  /* An offset to other data in the containing unit.  */
  ATTR_VAL_REF_UNIT,
  /* An offset to other data within the .debug_info section.  */
  ATTR_VAL_REF_INFO,
  /* An offset to other data within the alt .debug_info section.  */
  ATTR_VAL_REF_ALT_INFO,
  /* An offset to data in some other section.  */
  ATTR_VAL_REF_SECTION,
  /* A type signature.  */
  ATTR_VAL_REF_TYPE,
  /* An index into the .debug_rnglists section.  */
  ATTR_VAL_RNGLISTS_INDEX,
  /* A block of data (not represented).  */
  ATTR_VAL_BLOCK,
  /* An expression (not represented).  */
  ATTR_VAL_EXPR,
};

/* An attribute value.  */

struct attr_val
{
  /* How the value is stored in the field u.  */
  enum attr_val_encoding encoding;
  union
  {
    /* ATTR_VAL_ADDRESS*, ATTR_VAL_UINT, ATTR_VAL_REF*.  */
    uint64_t uint;
    /* ATTR_VAL_SINT.  */
    int64_t sint;
    /* ATTR_VAL_STRING.  */
    const char *string;
    /* ATTR_VAL_BLOCK not stored.  */
  } u;
};

/* The line number program header.  */

struct line_header
{
  /* The version of the line number information.  */
  int version;
  /* Address size.  */
  int addrsize;
  /* The minimum instruction length.  */
  unsigned int min_insn_len;
  /* The maximum number of ops per instruction.  */
  unsigned int max_ops_per_insn;
  /* The line base for special opcodes.  */
  int line_base;
  /* The line range for special opcodes.  */
  unsigned int line_range;
  /* The opcode base--the first special opcode.  */
  unsigned int opcode_base;
  /* Opcode lengths, indexed by opcode - 1.  */
  const unsigned char *opcode_lengths;
  /* The number of directory entries.  */
  size_t dirs_count;
  /* The directory entries.  */
  const char **dirs;
  /* The number of filenames.  */
  size_t filenames_count;
  /* The filenames.  */
  const char **filenames;
};

/* A format description from a line header.  */

struct line_header_format
{
  int lnct;		/* LNCT code.  */
  enum dwarf_form form;	/* Form of entry data.  */
};

/* Map a single PC value to a file/line.  We will keep a vector of
   these sorted by PC value.  Each file/line will be correct from the
   PC up to the PC of the next entry if there is one.  We allocate one
   extra entry at the end so that we can use bsearch.  */

struct line
{
  /* PC.  */
  uintptr_t pc;
  /* File name.  Many entries in the array are expected to point to
     the same file name.  */
  const char *filename;
  /* Line number.  */
  int lineno;
  /* Index of the object in the original array read from the DWARF
     section, before it has been sorted.  The index makes it possible
     to use Quicksort and maintain stability.  */
  int idx;
};

/* A growable vector of line number information.  This is used while
   reading the line numbers.  */

struct line_vector
{
  /* Memory.  This is an array of struct line.  */
  struct backtrace_vector vec;
  /* Number of valid mappings.  */
  size_t count;
};

/* A function described in the debug info.  */

struct function
{
  /* The name of the function.  */
  const char *name;
  /* If this is an inlined function, the filename of the call
     site.  */
  const char *caller_filename;
  /* If this is an inlined function, the line number of the call
     site.  */
  int caller_lineno;
  /* Map PC ranges to inlined functions.  */
  struct function_addrs *function_addrs;
  size_t function_addrs_count;
};

/* An address range for a function.  This maps a PC value to a
   specific function.  */

struct function_addrs
{
  /* Range is LOW <= PC < HIGH.  */
  uintptr_t low;
  uintptr_t high;
  /* Function for this address range.  */
  struct function *function;
};

/* A growable vector of function address ranges.  */

struct function_vector
{
  /* Memory.  This is an array of struct function_addrs.  */
  struct backtrace_vector vec;
  /* Number of address ranges present.  */
  size_t count;
};

/* A DWARF compilation unit.  This only holds the information we need
   to map a PC to a file and line.  */

struct unit
{
  /* The first entry for this compilation unit.  */
  const unsigned char *unit_data;
  /* The length of the data for this compilation unit.  */
  size_t unit_data_len;
  /* The offset of UNIT_DATA from the start of the information for
     this compilation unit.  */
  size_t unit_data_offset;
  /* Offset of the start of the compilation unit from the start of the
     .debug_info section.  */
  size_t low_offset;
  /* Offset of the end of the compilation unit from the start of the
     .debug_info section.  */
  size_t high_offset;
  /* DWARF version.  */
  int version;
  /* Whether unit is DWARF64.  */
  int is_dwarf64;
  /* Address size.  */
  int addrsize;
  /* Offset into line number information.  */
  off_t lineoff;
  /* Offset of compilation unit in .debug_str_offsets.  */
  uint64_t str_offsets_base;
  /* Offset of compilation unit in .debug_addr.  */
  uint64_t addr_base;
  /* Offset of compilation unit in .debug_rnglists.  */
  uint64_t rnglists_base;
  /* Primary source file.  */
  const char *filename;
  /* Compilation command working directory.  */
  const char *comp_dir;
  /* Absolute file name, only set if needed.  */
  const char *abs_filename;
  /* The abbreviations for this unit.  */
  struct abbrevs abbrevs;

  /* The fields above this point are read in during initialization and
     may be accessed freely.  The fields below this point are read in
     as needed, and therefore require care, as different threads may
     try to initialize them simultaneously.  */

  /* PC to line number mapping.  This is NULL if the values have not
     been read.  This is (struct line *) -1 if there was an error
     reading the values.  */
  struct line *lines;
  /* Number of entries in lines.  */
  size_t lines_count;
  /* PC ranges to function.  */
  struct function_addrs *function_addrs;
  size_t function_addrs_count;
};

/* An address range for a compilation unit.  This maps a PC value to a
   specific compilation unit.  Note that we invert the representation
   in DWARF: instead of listing the units and attaching a list of
   ranges, we list the ranges and have each one point to the unit.
   This lets us do a binary search to find the unit.  */

struct unit_addrs
{
  /* Range is LOW <= PC < HIGH.  */
  uintptr_t low;
  uintptr_t high;
  /* Compilation unit for this address range.  */
  struct unit *u;
};

/* A growable vector of compilation unit address ranges.  */

struct unit_addrs_vector
{
  /* Memory.  This is an array of struct unit_addrs.  */
  struct backtrace_vector vec;
  /* Number of address ranges present.  */
  size_t count;
};

/* A growable vector of compilation unit pointer.  */

struct unit_vector
{
  struct backtrace_vector vec;
  size_t count;
};

/* The information we need to map a PC to a file and line.  */

struct dwarf_data
{
  /* The data for the next file we know about.  */
  struct dwarf_data *next;
  /* The data for .gnu_debugaltlink.  */
  struct dwarf_data *altlink;
  /* The base address mapping for this file.  */
  struct libbacktrace_base_address base_address;
  /* A sorted list of address ranges.  */
  struct unit_addrs *addrs;
  /* Number of address ranges in list.  */
  size_t addrs_count;
  /* A sorted list of units.  */
  struct unit **units;
  /* Number of units in the list.  */
  size_t units_count;
  /* The unparsed DWARF debug data.  */
  struct dwarf_sections dwarf_sections;
  /* Whether the data is big-endian or not.  */
  int is_bigendian;
  /* A vector used for function addresses.  We keep this here so that
     we can grow the vector as we read more functions.  */
  struct function_vector fvec;
};

/* Report an error for a DWARF buffer.  */

static void
dwarf_buf_error (struct dwarf_buf *buf, const char *msg, int errnum)
{
  char b[200];

  snprintf (b, sizeof b, "%s in %s at %d",
	    msg, buf->name, (int) (buf->buf - buf->start));
  buf->error_callback (buf->data, b, errnum);
}

/* Require at least COUNT bytes in BUF.  Return 1 if all is well, 0 on
   error.  */

static int
require (struct dwarf_buf *buf, size_t count)
{
  if (buf->left >= count)
    return 1;

  if (!buf->reported_underflow)
    {
      dwarf_buf_error (buf, "DWARF underflow", 0);
      buf->reported_underflow = 1;
    }

  return 0;
}

/* Advance COUNT bytes in BUF.  Return 1 if all is well, 0 on
   error.  */

static int
advance (struct dwarf_buf *buf, size_t count)
{
  if (!require (buf, count))
    return 0;
  buf->buf += count;
  buf->left -= count;
  return 1;
}

/* Read one zero-terminated string from BUF and advance past the string.  */

static const char *
read_string (struct dwarf_buf *buf)
{
  const char *p = (const char *)buf->buf;
  size_t len = strnlen (p, buf->left);

  /* - If len == left, we ran out of buffer before finding the zero terminator.
       Generate an error by advancing len + 1.
     - If len < left, advance by len + 1 to skip past the zero terminator.  */
  size_t count = len + 1;

  if (!advance (buf, count))
    return NULL;

  return p;
}

/* Read one byte from BUF and advance 1 byte.  */

static unsigned char
read_byte (struct dwarf_buf *buf)
{
  const unsigned char *p = buf->buf;

  if (!advance (buf, 1))
    return 0;
  return p[0];
}

/* Read a signed char from BUF and advance 1 byte.  */

static signed char
read_sbyte (struct dwarf_buf *buf)
{
  const unsigned char *p = buf->buf;

  if (!advance (buf, 1))
    return 0;
  return (*p ^ 0x80) - 0x80;
}

/* Read a uint16 from BUF and advance 2 bytes.  */

static uint16_t
read_uint16 (struct dwarf_buf *buf)
{
  const unsigned char *p = buf->buf;

  if (!advance (buf, 2))
    return 0;
  if (buf->is_bigendian)
    return ((uint16_t) p[0] << 8) | (uint16_t) p[1];
  else
    return ((uint16_t) p[1] << 8) | (uint16_t) p[0];
}

/* Read a 24 bit value from BUF and advance 3 bytes.  */

static uint32_t
read_uint24 (struct dwarf_buf *buf)
{
  const unsigned char *p = buf->buf;

  if (!advance (buf, 3))
    return 0;
  if (buf->is_bigendian)
    return (((uint32_t) p[0] << 16) | ((uint32_t) p[1] << 8)
	    | (uint32_t) p[2]);
  else
    return (((uint32_t) p[2] << 16) | ((uint32_t) p[1] << 8)
	    | (uint32_t) p[0]);
}

/* Read a uint32 from BUF and advance 4 bytes.  */

static uint32_t
read_uint32 (struct dwarf_buf *buf)
{
  const unsigned char *p = buf->buf;

  if (!advance (buf, 4))
    return 0;
  if (buf->is_bigendian)
    return (((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16)
	    | ((uint32_t) p[2] << 8) | (uint32_t) p[3]);
  else
    return (((uint32_t) p[3] << 24) | ((uint32_t) p[2] << 16)
	    | ((uint32_t) p[1] << 8) | (uint32_t) p[0]);
}

/* Read a uint64 from BUF and advance 8 bytes.  */

static uint64_t
read_uint64 (struct dwarf_buf *buf)
{
  const unsigned char *p = buf->buf;

  if (!advance (buf, 8))
    return 0;
  if (buf->is_bigendian)
    return (((uint64_t) p[0] << 56) | ((uint64_t) p[1] << 48)
	    | ((uint64_t) p[2] << 40) | ((uint64_t) p[3] << 32)
	    | ((uint64_t) p[4] << 24) | ((uint64_t) p[5] << 16)
	    | ((uint64_t) p[6] << 8) | (uint64_t) p[7]);
  else
    return (((uint64_t) p[7] << 56) | ((uint64_t) p[6] << 48)
	    | ((uint64_t) p[5] << 40) | ((uint64_t) p[4] << 32)
	    | ((uint64_t) p[3] << 24) | ((uint64_t) p[2] << 16)
	    | ((uint64_t) p[1] << 8) | (uint64_t) p[0]);
}

/* Read an offset from BUF and advance the appropriate number of
   bytes.  */

static uint64_t
read_offset (struct dwarf_buf *buf, int is_dwarf64)
{
  if (is_dwarf64)
    return read_uint64 (buf);
  else
    return read_uint32 (buf);
}

/* Read an address from BUF and advance the appropriate number of
   bytes.  */

static uint64_t
read_address (struct dwarf_buf *buf, int addrsize)
{
  switch (addrsize)
    {
    case 1:
      return read_byte (buf);
    case 2:
      return read_uint16 (buf);
    case 4:
      return read_uint32 (buf);
    case 8:
      return read_uint64 (buf);
    default:
      dwarf_buf_error (buf, "unrecognized address size", 0);
      return 0;
    }
}

/* Return whether a value is the highest possible address, given the
   address size.  */

static int
is_highest_address (uint64_t address, int addrsize)
{
  switch (addrsize)
    {
    case 1:
      return address == (unsigned char) -1;
    case 2:
      return address == (uint16_t) -1;
    case 4:
      return address == (uint32_t) -1;
    case 8:
      return address == (uint64_t) -1;
    default:
      return 0;
    }
}

/* Read an unsigned LEB128 number.  */

static uint64_t
read_uleb128 (struct dwarf_buf *buf)
{
  uint64_t ret;
  unsigned int shift;
  int overflow;
  unsigned char b;

  ret = 0;
  shift = 0;
  overflow = 0;
  do
    {
      const unsigned char *p;

      p = buf->buf;
      if (!advance (buf, 1))
	return 0;
      b = *p;
      if (shift < 64)
	ret |= ((uint64_t) (b & 0x7f)) << shift;
      else if (!overflow)
	{
	  dwarf_buf_error (buf, "LEB128 overflows uint64_t", 0);
	  overflow = 1;
	}
      shift += 7;
    }
  while ((b & 0x80) != 0);

  return ret;
}

/* Read a signed LEB128 number.  */

static int64_t
read_sleb128 (struct dwarf_buf *buf)
{
  uint64_t val;
  unsigned int shift;
  int overflow;
  unsigned char b;

  val = 0;
  shift = 0;
  overflow = 0;
  do
    {
      const unsigned char *p;

      p = buf->buf;
      if (!advance (buf, 1))
	return 0;
      b = *p;
      if (shift < 64)
	val |= ((uint64_t) (b & 0x7f)) << shift;
      else if (!overflow)
	{
	  dwarf_buf_error (buf, "signed LEB128 overflows uint64_t", 0);
	  overflow = 1;
	}
      shift += 7;
    }
  while ((b & 0x80) != 0);

  if ((b & 0x40) != 0 && shift < 64)
    val |= ((uint64_t) -1) << shift;

  return (int64_t) val;
}

/* Return the length of an LEB128 number.  */

static size_t
leb128_len (const unsigned char *p)
{
  size_t ret;

  ret = 1;
  while ((*p & 0x80) != 0)
    {
      ++p;
      ++ret;
    }
  return ret;
}

/* Read initial_length from BUF and advance the appropriate number of bytes.  */

static uint64_t
read_initial_length (struct dwarf_buf *buf, int *is_dwarf64)
{
  uint64_t len;

  len = read_uint32 (buf);
  if (len == 0xffffffff)
    {
      len = read_uint64 (buf);
      *is_dwarf64 = 1;
    }
  else
    *is_dwarf64 = 0;

  return len;
}

/* Free an abbreviations structure.  */

static void
free_abbrevs (struct backtrace_state *state, struct abbrevs *abbrevs,
	      backtrace_error_callback error_callback, void *data)
{
  size_t i;

  for (i = 0; i < abbrevs->num_abbrevs; ++i)
    backtrace_free (state, abbrevs->abbrevs[i].attrs,
		    abbrevs->abbrevs[i].num_attrs * sizeof (struct attr),
		    error_callback, data);
  backtrace_free (state, abbrevs->abbrevs,
		  abbrevs->num_abbrevs * sizeof (struct abbrev),
		  error_callback, data);
  abbrevs->num_abbrevs = 0;
  abbrevs->abbrevs = NULL;
}

/* Read an attribute value.  Returns 1 on success, 0 on failure.  If
   the value can be represented as a uint64_t, sets *VAL and sets
   *IS_VALID to 1.  We don't try to store the value of other attribute
   forms, because we don't care about them.  */

static int
read_attribute (enum dwarf_form form, uint64_t implicit_val,
		struct dwarf_buf *buf, int is_dwarf64, int version,
		int addrsize, const struct dwarf_sections *dwarf_sections,
		struct dwarf_data *altlink, struct attr_val *val)
{
  /* Avoid warnings about val.u.FIELD may be used uninitialized if
     this function is inlined.  The warnings aren't valid but can
     occur because the different fields are set and used
     conditionally.  */
  memset (val, 0, sizeof *val);

  switch (form)
    {
    case DW_FORM_addr:
      val->encoding = ATTR_VAL_ADDRESS;
      val->u.uint = read_address (buf, addrsize);
      return 1;
    case DW_FORM_block2:
      val->encoding = ATTR_VAL_BLOCK;
      return advance (buf, read_uint16 (buf));
    case DW_FORM_block4:
      val->encoding = ATTR_VAL_BLOCK;
      return advance (buf, read_uint32 (buf));
    case DW_FORM_data2:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uint16 (buf);
      return 1;
    case DW_FORM_data4:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uint32 (buf);
      return 1;
    case DW_FORM_data8:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uint64 (buf);
      return 1;
    case DW_FORM_data16:
      val->encoding = ATTR_VAL_BLOCK;
      return advance (buf, 16);
    case DW_FORM_string:
      val->encoding = ATTR_VAL_STRING;
      val->u.string = read_string (buf);
      return val->u.string == NULL ? 0 : 1;
    case DW_FORM_block:
      val->encoding = ATTR_VAL_BLOCK;
      return advance (buf, read_uleb128 (buf));
    case DW_FORM_block1:
      val->encoding = ATTR_VAL_BLOCK;
      return advance (buf, read_byte (buf));
    case DW_FORM_data1:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_byte (buf);
      return 1;
    case DW_FORM_flag:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_byte (buf);
      return 1;
    case DW_FORM_sdata:
      val->encoding = ATTR_VAL_SINT;
      val->u.sint = read_sleb128 (buf);
      return 1;
    case DW_FORM_strp:
      {
	uint64_t offset;

	offset = read_offset (buf, is_dwarf64);
	if (offset >= dwarf_sections->size[DEBUG_STR])
	  {
	    dwarf_buf_error (buf, "DW_FORM_strp out of range", 0);
	    return 0;
	  }
	val->encoding = ATTR_VAL_STRING;
	val->u.string =
	  (const char *) dwarf_sections->data[DEBUG_STR] + offset;
	return 1;
      }
    case DW_FORM_line_strp:
      {
	uint64_t offset;

	offset = read_offset (buf, is_dwarf64);
	if (offset >= dwarf_sections->size[DEBUG_LINE_STR])
	  {
	    dwarf_buf_error (buf, "DW_FORM_line_strp out of range", 0);
	    return 0;
	  }
	val->encoding = ATTR_VAL_STRING;
	val->u.string =
	  (const char *) dwarf_sections->data[DEBUG_LINE_STR] + offset;
	return 1;
      }
    case DW_FORM_udata:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uleb128 (buf);
      return 1;
    case DW_FORM_ref_addr:
      val->encoding = ATTR_VAL_REF_INFO;
      if (version == 2)
	val->u.uint = read_address (buf, addrsize);
      else
	val->u.uint = read_offset (buf, is_dwarf64);
      return 1;
    case DW_FORM_ref1:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_byte (buf);
      return 1;
    case DW_FORM_ref2:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uint16 (buf);
      return 1;
    case DW_FORM_ref4:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uint32 (buf);
      return 1;
    case DW_FORM_ref8:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uint64 (buf);
      return 1;
    case DW_FORM_ref_udata:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uleb128 (buf);
      return 1;
    case DW_FORM_indirect:
      {
	uint64_t form;

	form = read_uleb128 (buf);
	if (form == DW_FORM_implicit_const)
	  {
	    dwarf_buf_error (buf,
			     "DW_FORM_indirect to DW_FORM_implicit_const",
			     0);
	    return 0;
	  }
	return read_attribute ((enum dwarf_form) form, 0, buf, is_dwarf64,
			       version, addrsize, dwarf_sections, altlink,
			       val);
      }
    case DW_FORM_sec_offset:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_offset (buf, is_dwarf64);
      return 1;
    case DW_FORM_exprloc:
      val->encoding = ATTR_VAL_EXPR;
      return advance (buf, read_uleb128 (buf));
    case DW_FORM_flag_present:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = 1;
      return 1;
    case DW_FORM_ref_sig8:
      val->encoding = ATTR_VAL_REF_TYPE;
      val->u.uint = read_uint64 (buf);
      return 1;
    case DW_FORM_strx: case DW_FORM_strx1: case DW_FORM_strx2:
    case DW_FORM_strx3: case DW_FORM_strx4:
      {
	uint64_t offset;

	switch (form)
	  {
	  case DW_FORM_strx:
	    offset = read_uleb128 (buf);
	    break;
	  case DW_FORM_strx1:
	    offset = read_byte (buf);
	    break;
	  case DW_FORM_strx2:
	    offset = read_uint16 (buf);
	    break;
	  case DW_FORM_strx3:
	    offset = read_uint24 (buf);
	    break;
	  case DW_FORM_strx4:
	    offset = read_uint32 (buf);
	    break;
	  default:
	    /* This case can't happen.  */
	    return 0;
	  }
	val->encoding = ATTR_VAL_STRING_INDEX;
	val->u.uint = offset;
	return 1;
      }
    case DW_FORM_addrx: case DW_FORM_addrx1: case DW_FORM_addrx2:
    case DW_FORM_addrx3: case DW_FORM_addrx4:
      {
	uint64_t offset;

	switch (form)
	  {
	  case DW_FORM_addrx:
	    offset = read_uleb128 (buf);
	    break;
	  case DW_FORM_addrx1:
	    offset = read_byte (buf);
	    break;
	  case DW_FORM_addrx2:
	    offset = read_uint16 (buf);
	    break;
	  case DW_FORM_addrx3:
	    offset = read_uint24 (buf);
	    break;
	  case DW_FORM_addrx4:
	    offset = read_uint32 (buf);
	    break;
	  default:
	    /* This case can't happen.  */
	    return 0;
	  }
	val->encoding = ATTR_VAL_ADDRESS_INDEX;
	val->u.uint = offset;
	return 1;
      }
    case DW_FORM_ref_sup4:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uint32 (buf);
      return 1;
    case DW_FORM_ref_sup8:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uint64 (buf);
      return 1;
    case DW_FORM_implicit_const:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = implicit_val;
      return 1;
    case DW_FORM_loclistx:
      /* We don't distinguish this from DW_FORM_sec_offset.  It
       * shouldn't matter since we don't care about loclists.  */
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uleb128 (buf);
      return 1;
    case DW_FORM_rnglistx:
      val->encoding = ATTR_VAL_RNGLISTS_INDEX;
      val->u.uint = read_uleb128 (buf);
      return 1;
    case DW_FORM_GNU_addr_index:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uleb128 (buf);
      return 1;
    case DW_FORM_GNU_str_index:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uleb128 (buf);
      return 1;
    case DW_FORM_GNU_ref_alt:
      val->u.uint = read_offset (buf, is_dwarf64);
      if (altlink == NULL)
	{
	  val->encoding = ATTR_VAL_NONE;
	  return 1;
	}
      val->encoding = ATTR_VAL_REF_ALT_INFO;
      return 1;
    case DW_FORM_strp_sup: case DW_FORM_GNU_strp_alt:
      {
	uint64_t offset;

	offset = read_offset (buf, is_dwarf64);
	if (altlink == NULL)
	  {
	    val->encoding = ATTR_VAL_NONE;
	    return 1;
	  }
	if (offset >= altlink->dwarf_sections.size[DEBUG_STR])
	  {
	    dwarf_buf_error (buf, "DW_FORM_strp_sup out of range", 0);
	    return 0;
	  }
	val->encoding = ATTR_VAL_STRING;
	val->u.string =
	  (const char *) altlink->dwarf_sections.data[DEBUG_STR] + offset;
	return 1;
      }
    default:
      dwarf_buf_error (buf, "unrecognized DWARF form", -1);
      return 0;
    }
}

/* If we can determine the value of a string attribute, set *STRING to
   point to the string.  Return 1 on success, 0 on error.  If we don't
   know the value, we consider that a success, and we don't change
   *STRING.  An error is only reported for some sort of out of range
   offset.  */

static int
resolve_string (const struct dwarf_sections *dwarf_sections, int is_dwarf64,
		int is_bigendian, uint64_t str_offsets_base,
		const struct attr_val *val,
		backtrace_error_callback error_callback, void *data,
		const char **string)
{
  switch (val->encoding)
    {
    case ATTR_VAL_STRING:
      *string = val->u.string;
      return 1;

    case ATTR_VAL_STRING_INDEX:
      {
	uint64_t offset;
	struct dwarf_buf offset_buf;

	offset = val->u.uint * (is_dwarf64 ? 8 : 4) + str_offsets_base;
	if (offset + (is_dwarf64 ? 8 : 4)
	    > dwarf_sections->size[DEBUG_STR_OFFSETS])
	  {
	    error_callback (data, "DW_FORM_strx value out of range", 0);
	    return 0;
	  }

	offset_buf.name = ".debug_str_offsets";
	offset_buf.start = dwarf_sections->data[DEBUG_STR_OFFSETS];
	offset_buf.buf = dwarf_sections->data[DEBUG_STR_OFFSETS] + offset;
	offset_buf.left = dwarf_sections->size[DEBUG_STR_OFFSETS] - offset;
	offset_buf.is_bigendian = is_bigendian;
	offset_buf.error_callback = error_callback;
	offset_buf.data = data;
	offset_buf.reported_underflow = 0;

	offset = read_offset (&offset_buf, is_dwarf64);
	if (offset >= dwarf_sections->size[DEBUG_STR])
	  {
	    dwarf_buf_error (&offset_buf,
			     "DW_FORM_strx offset out of range",
			     0);
	    return 0;
	  }
	*string = (const char *) dwarf_sections->data[DEBUG_STR] + offset;
	return 1;
      }

    default:
      return 1;
    }
}

/* Set *ADDRESS to the real address for a ATTR_VAL_ADDRESS_INDEX.
   Return 1 on success, 0 on error.  */

static int
resolve_addr_index (const struct dwarf_sections *dwarf_sections,
		    uint64_t addr_base, int addrsize, int is_bigendian,
		    uint64_t addr_index,
		    backtrace_error_callback error_callback, void *data,
		    uintptr_t *address)
{
  uint64_t offset;
  struct dwarf_buf addr_buf;

  offset = addr_index * addrsize + addr_base;
  if (offset + addrsize > dwarf_sections->size[DEBUG_ADDR])
    {
      error_callback (data, "DW_FORM_addrx value out of range", 0);
      return 0;
    }

  addr_buf.name = ".debug_addr";
  addr_buf.start = dwarf_sections->data[DEBUG_ADDR];
  addr_buf.buf = dwarf_sections->data[DEBUG_ADDR] + offset;
  addr_buf.left = dwarf_sections->size[DEBUG_ADDR] - offset;
  addr_buf.is_bigendian = is_bigendian;
  addr_buf.error_callback = error_callback;
  addr_buf.data = data;
  addr_buf.reported_underflow = 0;

  *address = (uintptr_t) read_address (&addr_buf, addrsize);
  return 1;
}

/* Compare a unit offset against a unit for bsearch.  */

static int
units_search (const void *vkey, const void *ventry)
{
  const size_t *key = (const size_t *) vkey;
  const struct unit *entry = *((const struct unit *const *) ventry);
  size_t offset;

  offset = *key;
  if (offset < entry->low_offset)
    return -1;
  else if (offset >= entry->high_offset)
    return 1;
  else
    return 0;
}

/* Find a unit in PU containing OFFSET.  */

static struct unit *
find_unit (struct unit **pu, size_t units_count, size_t offset)
{
  struct unit **u;
  u = bsearch (&offset, pu, units_count, sizeof (struct unit *), units_search);
  return u == NULL ? NULL : *u;
}

/* Compare function_addrs for qsort.  When ranges are nested, make the
   smallest one sort last.  */

static int
function_addrs_compare (const void *v1, const void *v2)
{
  const struct function_addrs *a1 = (const struct function_addrs *) v1;
  const struct function_addrs *a2 = (const struct function_addrs *) v2;

  if (a1->low < a2->low)
    return -1;
  if (a1->low > a2->low)
    return 1;
  if (a1->high < a2->high)
    return 1;
  if (a1->high > a2->high)
    return -1;
  return strcmp (a1->function->name, a2->function->name);
}

/* Compare a PC against a function_addrs for bsearch.  We always
   allocate an entra entry at the end of the vector, so that this
   routine can safely look at the next entry.  Note that if there are
   multiple ranges containing PC, which one will be returned is
   unpredictable.  We compensate for that in dwarf_fileline.  */

static int
function_addrs_search (const void *vkey, const void *ventry)
{
  const uintptr_t *key = (const uintptr_t *) vkey;
  const struct function_addrs *entry = (const struct function_addrs *) ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->low)
    return -1;
  else if (pc > (entry + 1)->low)
    return 1;
  else
    return 0;
}

/* Add a new compilation unit address range to a vector.  This is
   called via add_ranges.  Returns 1 on success, 0 on failure.  */

static int
add_unit_addr (struct backtrace_state *state, void *rdata,
	       uintptr_t lowpc, uintptr_t highpc,
	       backtrace_error_callback error_callback, void *data,
	       void *pvec)
{
  struct unit *u = (struct unit *) rdata;
  struct unit_addrs_vector *vec = (struct unit_addrs_vector *) pvec;
  struct unit_addrs *p;

  /* Try to merge with the last entry.  */
  if (vec->count > 0)
    {
      p = (struct unit_addrs *) vec->vec.base + (vec->count - 1);
      if ((lowpc == p->high || lowpc == p->high + 1)
	  && u == p->u)
	{
	  if (highpc > p->high)
	    p->high = highpc;
	  return 1;
	}
    }

  p = ((struct unit_addrs *)
       backtrace_vector_grow (state, sizeof (struct unit_addrs),
			      error_callback, data, &vec->vec));
  if (p == NULL)
    return 0;

  p->low = lowpc;
  p->high = highpc;
  p->u = u;

  ++vec->count;

  return 1;
}

/* Compare unit_addrs for qsort.  When ranges are nested, make the
   smallest one sort last.  */

static int
unit_addrs_compare (const void *v1, const void *v2)
{
  const struct unit_addrs *a1 = (const struct unit_addrs *) v1;
  const struct unit_addrs *a2 = (const struct unit_addrs *) v2;

  if (a1->low < a2->low)
    return -1;
  if (a1->low > a2->low)
    return 1;
  if (a1->high < a2->high)
    return 1;
  if (a1->high > a2->high)
    return -1;
  if (a1->u->lineoff < a2->u->lineoff)
    return -1;
  if (a1->u->lineoff > a2->u->lineoff)
    return 1;
  return 0;
}

/* Compare a PC against a unit_addrs for bsearch.  We always allocate
   an entry entry at the end of the vector, so that this routine can
   safely look at the next entry.  Note that if there are multiple
   ranges containing PC, which one will be returned is unpredictable.
   We compensate for that in dwarf_fileline.  */

static int
unit_addrs_search (const void *vkey, const void *ventry)
{
  const uintptr_t *key = (const uintptr_t *) vkey;
  const struct unit_addrs *entry = (const struct unit_addrs *) ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->low)
    return -1;
  else if (pc > (entry + 1)->low)
    return 1;
  else
    return 0;
}

/* Fill in overlapping ranges as needed.  This is a subroutine of
   resolve_unit_addrs_overlap.  */

static int
resolve_unit_addrs_overlap_walk (struct backtrace_state *state,
				 size_t *pfrom, size_t *pto,
				 struct unit_addrs *enclosing,
				 struct unit_addrs_vector *old_vec,
				 backtrace_error_callback error_callback,
				 void *data,
				 struct unit_addrs_vector *new_vec)
{
  struct unit_addrs *old_addrs;
  size_t old_count;
  struct unit_addrs *new_addrs;
  size_t from;
  size_t to;

  old_addrs = (struct unit_addrs *) old_vec->vec.base;
  old_count = old_vec->count;
  new_addrs = (struct unit_addrs *) new_vec->vec.base;

  for (from = *pfrom, to = *pto; from < old_count; from++, to++)
    {
      /* If we are in the scope of a larger range that can no longer
	 cover any further ranges, return back to the caller.  */

      if (enclosing != NULL
	  && enclosing->high <= old_addrs[from].low)
	{
	  *pfrom = from;
	  *pto = to;
	  return 1;
	}

      new_addrs[to] = old_addrs[from];

      /* If we are in scope of a larger range, fill in any gaps
	 between this entry and the next one.

	 There is an extra entry at the end of the vector, so it's
	 always OK to refer to from + 1.  */

      if (enclosing != NULL
	  && enclosing->high > old_addrs[from].high
	  && old_addrs[from].high < old_addrs[from + 1].low)
	{
	  void *grew;
	  size_t new_high;

	  grew = backtrace_vector_grow (state, sizeof (struct unit_addrs),
					error_callback, data, &new_vec->vec);
	  if (grew == NULL)
	    return 0;
	  new_addrs = (struct unit_addrs *) new_vec->vec.base;
	  to++;
	  new_addrs[to].low = old_addrs[from].high;
	  new_high = old_addrs[from + 1].low;
	  if (enclosing->high < new_high)
	    new_high = enclosing->high;
	  new_addrs[to].high = new_high;
	  new_addrs[to].u = enclosing->u;
	}

      /* If this range has a larger scope than the next one, use it to
	 fill in any gaps.  */

      if (old_addrs[from].high > old_addrs[from + 1].high)
	{
	  *pfrom = from + 1;
	  *pto = to + 1;
	  if (!resolve_unit_addrs_overlap_walk (state, pfrom, pto,
						&old_addrs[from], old_vec,
						error_callback, data, new_vec))
	    return 0;
	  from = *pfrom;
	  to = *pto;

	  /* Undo the increment the loop is about to do.  */
	  from--;
	  to--;
	}
    }

  if (enclosing == NULL)
    {
      struct unit_addrs *pa;

      /* Add trailing entry.  */

      pa = ((struct unit_addrs *)
	    backtrace_vector_grow (state, sizeof (struct unit_addrs),
				   error_callback, data, &new_vec->vec));
      if (pa == NULL)
	return 0;
      pa->low = 0;
      --pa->low;
      pa->high = pa->low;
      pa->u = NULL;

      new_vec->count = to;
    }

  return 1;
}

/* It is possible for the unit_addrs list to contain overlaps, as in

       10: low == 10, high == 20, unit 1
       11: low == 12, high == 15, unit 2
       12: low == 20, high == 30, unit 1

   In such a case, for pc == 17, a search using units_addr_search will
   return entry 11.  However, pc == 17 doesn't fit in that range.  We
   actually want range 10.

   It seems that in general we might have an arbitrary number of
   ranges in between 10 and 12.

   To handle this we look for cases where range R1 is followed by
   range R2 such that R2 is a strict subset of R1.  In such cases we
   insert a new range R3 following R2 that fills in the remainder of
   the address space covered by R1.  That lets a relatively simple
   search find the correct range.

   These overlaps can occur because of the range merging we do in
   add_unit_addr.  When the linker de-duplicates functions, it can
   leave behind an address range that refers to the address range of
   the retained duplicate.  If the retained duplicate address range is
   merged with others, then after sorting we can see overlapping
   address ranges.

   See https://github.com/ianlancetaylor/libbacktrace/issues/137.  */

static int
resolve_unit_addrs_overlap (struct backtrace_state *state,
			    backtrace_error_callback error_callback,
			    void *data, struct unit_addrs_vector *addrs_vec)
{
  struct unit_addrs *addrs;
  size_t count;
  int found;
  struct unit_addrs *entry;
  size_t i;
  struct unit_addrs_vector new_vec;
  void *grew;
  size_t from;
  size_t to;

  addrs = (struct unit_addrs *) addrs_vec->vec.base;
  count = addrs_vec->count;

  if (count == 0)
    return 1;

  /* Optimistically assume that overlaps are rare.  */
  found = 0;
  entry = addrs;
  for (i = 0; i < count - 1; i++)
    {
      if (entry->low < (entry + 1)->low
	  && entry->high > (entry + 1)->high)
	{
	  found = 1;
	  break;
	}
      entry++;
    }
  if (!found)
    return 1;

  memset (&new_vec, 0, sizeof new_vec);
  grew = backtrace_vector_grow (state,
				count * sizeof (struct unit_addrs),
				error_callback, data, &new_vec.vec);
  if (grew == NULL)
    return 0;

  from = 0;
  to = 0;
  resolve_unit_addrs_overlap_walk (state, &from, &to, NULL, addrs_vec,
				   error_callback, data, &new_vec);
  backtrace_vector_free (state, &addrs_vec->vec, error_callback, data);
  *addrs_vec = new_vec;

  return 1;
}

/* Sort the line vector by PC.  We want a stable sort here to maintain
   the order of lines for the same PC values.  Since the sequence is
   being sorted in place, their addresses cannot be relied on to
   maintain stability.  That is the purpose of the index member.  */

static int
line_compare (const void *v1, const void *v2)
{
  const struct line *ln1 = (const struct line *) v1;
  const struct line *ln2 = (const struct line *) v2;

  if (ln1->pc < ln2->pc)
    return -1;
  else if (ln1->pc > ln2->pc)
    return 1;
  else if (ln1->idx < ln2->idx)
    return -1;
  else if (ln1->idx > ln2->idx)
    return 1;
  else
    return 0;
}

/* Find a PC in a line vector.  We always allocate an extra entry at
   the end of the lines vector, so that this routine can safely look
   at the next entry.  Note that when there are multiple mappings for
   the same PC value, this will return the last one.  */

static int
line_search (const void *vkey, const void *ventry)
{
  const uintptr_t *key = (const uintptr_t *) vkey;
  const struct line *entry = (const struct line *) ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->pc)
    return -1;
  else if (pc >= (entry + 1)->pc)
    return 1;
  else
    return 0;
}

/* Sort the abbrevs by the abbrev code.  This function is passed to
   both qsort and bsearch.  */

static int
abbrev_compare (const void *v1, const void *v2)
{
  const struct abbrev *a1 = (const struct abbrev *) v1;
  const struct abbrev *a2 = (const struct abbrev *) v2;

  if (a1->code < a2->code)
    return -1;
  else if (a1->code > a2->code)
    return 1;
  else
    {
      /* This really shouldn't happen.  It means there are two
	 different abbrevs with the same code, and that means we don't
	 know which one lookup_abbrev should return.  */
      return 0;
    }
}

/* Read the abbreviation table for a compilation unit.  Returns 1 on
   success, 0 on failure.  */

static int
read_abbrevs (struct backtrace_state *state, uint64_t abbrev_offset,
	      const unsigned char *dwarf_abbrev, size_t dwarf_abbrev_size,
	      int is_bigendian, backtrace_error_callback error_callback,
	      void *data, struct abbrevs *abbrevs)
{
  struct dwarf_buf abbrev_buf;
  struct dwarf_buf count_buf;
  size_t num_abbrevs;

  abbrevs->num_abbrevs = 0;
  abbrevs->abbrevs = NULL;

  if (abbrev_offset >= dwarf_abbrev_size)
    {
      error_callback (data, "abbrev offset out of range", 0);
      return 0;
    }

  abbrev_buf.name = ".debug_abbrev";
  abbrev_buf.start = dwarf_abbrev;
  abbrev_buf.buf = dwarf_abbrev + abbrev_offset;
  abbrev_buf.left = dwarf_abbrev_size - abbrev_offset;
  abbrev_buf.is_bigendian = is_bigendian;
  abbrev_buf.error_callback = error_callback;
  abbrev_buf.data = data;
  abbrev_buf.reported_underflow = 0;

  /* Count the number of abbrevs in this list.  */

  count_buf = abbrev_buf;
  num_abbrevs = 0;
  while (read_uleb128 (&count_buf) != 0)
    {
      if (count_buf.reported_underflow)
	return 0;
      ++num_abbrevs;
      // Skip tag.
      read_uleb128 (&count_buf);
      // Skip has_children.
      read_byte (&count_buf);
      // Skip attributes.
      while (read_uleb128 (&count_buf) != 0)
	{
	  uint64_t form;

	  form = read_uleb128 (&count_buf);
	  if ((enum dwarf_form) form == DW_FORM_implicit_const)
	    read_sleb128 (&count_buf);
	}
      // Skip form of last attribute.
      read_uleb128 (&count_buf);
    }

  if (count_buf.reported_underflow)
    return 0;

  if (num_abbrevs == 0)
    return 1;

  abbrevs->abbrevs = ((struct abbrev *)
		      backtrace_alloc (state,
				       num_abbrevs * sizeof (struct abbrev),
				       error_callback, data));
  if (abbrevs->abbrevs == NULL)
    return 0;
  abbrevs->num_abbrevs = num_abbrevs;
  memset (abbrevs->abbrevs, 0, num_abbrevs * sizeof (struct abbrev));

  num_abbrevs = 0;
  while (1)
    {
      uint64_t code;
      struct abbrev a;
      size_t num_attrs;
      struct attr *attrs;

      if (abbrev_buf.reported_underflow)
	goto fail;

      code = read_uleb128 (&abbrev_buf);
      if (code == 0)
	break;

      a.code = code;
      a.tag = (enum dwarf_tag) read_uleb128 (&abbrev_buf);
      a.has_children = read_byte (&abbrev_buf);

      count_buf = abbrev_buf;
      num_attrs = 0;
      while (read_uleb128 (&count_buf) != 0)
	{
	  uint64_t form;

	  ++num_attrs;
	  form = read_uleb128 (&count_buf);
	  if ((enum dwarf_form) form == DW_FORM_implicit_const)
	    read_sleb128 (&count_buf);
	}

      if (num_attrs == 0)
	{
	  attrs = NULL;
	  read_uleb128 (&abbrev_buf);
	  read_uleb128 (&abbrev_buf);
	}
      else
	{
	  attrs = ((struct attr *)
		   backtrace_alloc (state, num_attrs * sizeof *attrs,
				    error_callback, data));
	  if (attrs == NULL)
	    goto fail;
	  num_attrs = 0;
	  while (1)
	    {
	      uint64_t name;
	      uint64_t form;

	      name = read_uleb128 (&abbrev_buf);
	      form = read_uleb128 (&abbrev_buf);
	      if (name == 0)
		break;
	      attrs[num_attrs].name = (enum dwarf_attribute) name;
	      attrs[num_attrs].form = (enum dwarf_form) form;
	      if ((enum dwarf_form) form == DW_FORM_implicit_const)
		attrs[num_attrs].val = read_sleb128 (&abbrev_buf);
	      else
		attrs[num_attrs].val = 0;
	      ++num_attrs;
	    }
	}

      a.num_attrs = num_attrs;
      a.attrs = attrs;

      abbrevs->abbrevs[num_abbrevs] = a;
      ++num_abbrevs;
    }

  backtrace_qsort (abbrevs->abbrevs, abbrevs->num_abbrevs,
		   sizeof (struct abbrev), abbrev_compare);

  return 1;

 fail:
  free_abbrevs (state, abbrevs, error_callback, data);
  return 0;
}

/* Return the abbrev information for an abbrev code.  */

static const struct abbrev *
lookup_abbrev (struct abbrevs *abbrevs, uint64_t code,
	       backtrace_error_callback error_callback, void *data)
{
  struct abbrev key;
  void *p;

  /* With GCC, where abbrevs are simply numbered in order, we should
     be able to just look up the entry.  */
  if (code - 1 < abbrevs->num_abbrevs
      && abbrevs->abbrevs[code - 1].code == code)
    return &abbrevs->abbrevs[code - 1];

  /* Otherwise we have to search.  */
  memset (&key, 0, sizeof key);
  key.code = code;
  p = bsearch (&key, abbrevs->abbrevs, abbrevs->num_abbrevs,
	       sizeof (struct abbrev), abbrev_compare);
  if (p == NULL)
    {
      error_callback (data, "invalid abbreviation code", 0);
      return NULL;
    }
  return (const struct abbrev *) p;
}

/* This struct is used to gather address range information while
   reading attributes.  We use this while building a mapping from
   address ranges to compilation units and then again while mapping
   from address ranges to function entries.  Normally either
   lowpc/highpc is set or ranges is set.  */

struct pcrange {
  uintptr_t lowpc;             /* The low PC value.  */
  int have_lowpc;		/* Whether a low PC value was found.  */
  int lowpc_is_addr_index;	/* Whether lowpc is in .debug_addr.  */
  uintptr_t highpc;            /* The high PC value.  */
  int have_highpc;		/* Whether a high PC value was found.  */
  int highpc_is_relative;	/* Whether highpc is relative to lowpc.  */
  int highpc_is_addr_index;	/* Whether highpc is in .debug_addr.  */
  uint64_t ranges;		/* Offset in ranges section.  */
  int have_ranges;		/* Whether ranges is valid.  */
  int ranges_is_index;		/* Whether ranges is DW_FORM_rnglistx.  */
};

/* Update PCRANGE from an attribute value.  */

static void
update_pcrange (const struct attr* attr, const struct attr_val* val,
		struct pcrange *pcrange)
{
  switch (attr->name)
    {
    case DW_AT_low_pc:
      if (val->encoding == ATTR_VAL_ADDRESS)
	{
	  pcrange->lowpc = (uintptr_t) val->u.uint;
	  pcrange->have_lowpc = 1;
	}
      else if (val->encoding == ATTR_VAL_ADDRESS_INDEX)
	{
	  pcrange->lowpc = (uintptr_t) val->u.uint;
	  pcrange->have_lowpc = 1;
	  pcrange->lowpc_is_addr_index = 1;
	}
      break;

    case DW_AT_high_pc:
      if (val->encoding == ATTR_VAL_ADDRESS)
	{
	  pcrange->highpc = (uintptr_t) val->u.uint;
	  pcrange->have_highpc = 1;
	}
      else if (val->encoding == ATTR_VAL_UINT)
	{
	  pcrange->highpc = (uintptr_t) val->u.uint;
	  pcrange->have_highpc = 1;
	  pcrange->highpc_is_relative = 1;
	}
      else if (val->encoding == ATTR_VAL_ADDRESS_INDEX)
	{
	  pcrange->highpc = (uintptr_t) val->u.uint;
	  pcrange->have_highpc = 1;
	  pcrange->highpc_is_addr_index = 1;
	}
      break;

    case DW_AT_ranges:
      if (val->encoding == ATTR_VAL_UINT
	  || val->encoding == ATTR_VAL_REF_SECTION)
	{
	  pcrange->ranges = val->u.uint;
	  pcrange->have_ranges = 1;
	}
      else if (val->encoding == ATTR_VAL_RNGLISTS_INDEX)
	{
	  pcrange->ranges = val->u.uint;
	  pcrange->have_ranges = 1;
	  pcrange->ranges_is_index = 1;
	}
      break;

    default:
      break;
    }
}

/* Call ADD_RANGE for a low/high PC pair.  Returns 1 on success, 0 on
  error.  */

static int
add_low_high_range (struct backtrace_state *state,
		    const struct dwarf_sections *dwarf_sections,
		    struct libbacktrace_base_address base_address,
		    int is_bigendian, struct unit *u,
		    const struct pcrange *pcrange,
		    int (*add_range) (struct backtrace_state *state,
				      void *rdata, uintptr_t lowpc,
				      uintptr_t highpc,
				      backtrace_error_callback error_callback,
				      void *data, void *vec),
		    void *rdata,
		    backtrace_error_callback error_callback, void *data,
		    void *vec)
{
  uintptr_t lowpc;
  uintptr_t highpc;

  lowpc = pcrange->lowpc;
  if (pcrange->lowpc_is_addr_index)
    {
      if (!resolve_addr_index (dwarf_sections, u->addr_base, u->addrsize,
			       is_bigendian, lowpc, error_callback, data,
			       &lowpc))
	return 0;
    }

  highpc = pcrange->highpc;
  if (pcrange->highpc_is_addr_index)
    {
      if (!resolve_addr_index (dwarf_sections, u->addr_base, u->addrsize,
			       is_bigendian, highpc, error_callback, data,
			       &highpc))
	return 0;
    }
  if (pcrange->highpc_is_relative)
    highpc += lowpc;

  /* Add in the base address of the module when recording PC values,
     so that we can look up the PC directly.  */
  lowpc = libbacktrace_add_base (lowpc, base_address);
  highpc = libbacktrace_add_base (highpc, base_address);

  return add_range (state, rdata, lowpc, highpc, error_callback, data, vec);
}

/* Call ADD_RANGE for each range read from .debug_ranges, as used in
   DWARF versions 2 through 4.  */

static int
add_ranges_from_ranges (
    struct backtrace_state *state,
    const struct dwarf_sections *dwarf_sections,
    struct libbacktrace_base_address base_address, int is_bigendian,
    struct unit *u, uintptr_t base,
    const struct pcrange *pcrange,
    int (*add_range) (struct backtrace_state *state, void *rdata,
		      uintptr_t lowpc, uintptr_t highpc,
		      backtrace_error_callback error_callback, void *data,
		      void *vec),
    void *rdata,
    backtrace_error_callback error_callback, void *data,
    void *vec)
{
  struct dwarf_buf ranges_buf;

  if (pcrange->ranges >= dwarf_sections->size[DEBUG_RANGES])
    {
      error_callback (data, "ranges offset out of range", 0);
      return 0;
    }

  ranges_buf.name = ".debug_ranges";
  ranges_buf.start = dwarf_sections->data[DEBUG_RANGES];
  ranges_buf.buf = dwarf_sections->data[DEBUG_RANGES] + pcrange->ranges;
  ranges_buf.left = dwarf_sections->size[DEBUG_RANGES] - pcrange->ranges;
  ranges_buf.is_bigendian = is_bigendian;
  ranges_buf.error_callback = error_callback;
  ranges_buf.data = data;
  ranges_buf.reported_underflow = 0;

  while (1)
    {
      uint64_t low;
      uint64_t high;

      if (ranges_buf.reported_underflow)
	return 0;

      low = read_address (&ranges_buf, u->addrsize);
      high = read_address (&ranges_buf, u->addrsize);

      if (low == 0 && high == 0)
	break;

      if (is_highest_address (low, u->addrsize))
	base = (uintptr_t) high;
      else
	{
	  uintptr_t rl, rh;

	  rl = libbacktrace_add_base ((uintptr_t) low + base, base_address);
	  rh = libbacktrace_add_base ((uintptr_t) high + base, base_address);
	  if (!add_range (state, rdata, rl, rh, error_callback, data, vec))
	    return 0;
	}
    }

  if (ranges_buf.reported_underflow)
    return 0;

  return 1;
}

/* Call ADD_RANGE for each range read from .debug_rnglists, as used in
   DWARF version 5.  */

static int
add_ranges_from_rnglists (
    struct backtrace_state *state,
    const struct dwarf_sections *dwarf_sections,
    struct libbacktrace_base_address base_address, int is_bigendian,
    struct unit *u, uintptr_t base,
    const struct pcrange *pcrange,
    int (*add_range) (struct backtrace_state *state, void *rdata,
		      uintptr_t lowpc, uintptr_t highpc,
		      backtrace_error_callback error_callback, void *data,
		      void *vec),
    void *rdata,
    backtrace_error_callback error_callback, void *data,
    void *vec)
{
  uint64_t offset;
  struct dwarf_buf rnglists_buf;

  if (!pcrange->ranges_is_index)
    offset = pcrange->ranges;
  else
    offset = u->rnglists_base + pcrange->ranges * (u->is_dwarf64 ? 8 : 4);
  if (offset >= dwarf_sections->size[DEBUG_RNGLISTS])
    {
      error_callback (data, "rnglists offset out of range", 0);
      return 0;
    }

  rnglists_buf.name = ".debug_rnglists";
  rnglists_buf.start = dwarf_sections->data[DEBUG_RNGLISTS];
  rnglists_buf.buf = dwarf_sections->data[DEBUG_RNGLISTS] + offset;
  rnglists_buf.left = dwarf_sections->size[DEBUG_RNGLISTS] - offset;
  rnglists_buf.is_bigendian = is_bigendian;
  rnglists_buf.error_callback = error_callback;
  rnglists_buf.data = data;
  rnglists_buf.reported_underflow = 0;

  if (pcrange->ranges_is_index)
    {
      offset = read_offset (&rnglists_buf, u->is_dwarf64);
      offset += u->rnglists_base;
      if (offset >= dwarf_sections->size[DEBUG_RNGLISTS])
	{
	  error_callback (data, "rnglists index offset out of range", 0);
	  return 0;
	}
      rnglists_buf.buf = dwarf_sections->data[DEBUG_RNGLISTS] + offset;
      rnglists_buf.left = dwarf_sections->size[DEBUG_RNGLISTS] - offset;
    }

  while (1)
    {
      unsigned char rle;

      rle = read_byte (&rnglists_buf);
      if (rle == DW_RLE_end_of_list)
	break;
      switch (rle)
	{
	case DW_RLE_base_addressx:
	  {
	    uint64_t index;

	    index = read_uleb128 (&rnglists_buf);
	    if (!resolve_addr_index (dwarf_sections, u->addr_base,
				     u->addrsize, is_bigendian, index,
				     error_callback, data, &base))
	      return 0;
	  }
	  break;

	case DW_RLE_startx_endx:
	  {
	    uint64_t index;
	    uintptr_t low;
	    uintptr_t high;

	    index = read_uleb128 (&rnglists_buf);
	    if (!resolve_addr_index (dwarf_sections, u->addr_base,
				     u->addrsize, is_bigendian, index,
				     error_callback, data, &low))
	      return 0;
	    index = read_uleb128 (&rnglists_buf);
	    if (!resolve_addr_index (dwarf_sections, u->addr_base,
				     u->addrsize, is_bigendian, index,
				     error_callback, data, &high))
	      return 0;
	    if (!add_range (state, rdata,
			    libbacktrace_add_base (low, base_address),
			    libbacktrace_add_base (high, base_address),
			    error_callback, data, vec))
	      return 0;
	  }
	  break;

	case DW_RLE_startx_length:
	  {
	    uint64_t index;
	    uintptr_t low;
	    uintptr_t length;

	    index = read_uleb128 (&rnglists_buf);
	    if (!resolve_addr_index (dwarf_sections, u->addr_base,
				     u->addrsize, is_bigendian, index,
				     error_callback, data, &low))
	      return 0;
	    length = read_uleb128 (&rnglists_buf);
	    low = libbacktrace_add_base (low, base_address);
	    if (!add_range (state, rdata, low, low + length,
			    error_callback, data, vec))
	      return 0;
	  }
	  break;

	case DW_RLE_offset_pair:
	  {
	    uint64_t low;
	    uint64_t high;

	    low = read_uleb128 (&rnglists_buf);
	    high = read_uleb128 (&rnglists_buf);
	    if (!add_range (state, rdata,
			    libbacktrace_add_base (low + base, base_address),
			    libbacktrace_add_base (high + base, base_address),
			    error_callback, data, vec))
	      return 0;
	  }
	  break;

	case DW_RLE_base_address:
	  base = (uintptr_t) read_address (&rnglists_buf, u->addrsize);
	  break;

	case DW_RLE_start_end:
	  {
	    uintptr_t low;
	    uintptr_t high;

	    low = (uintptr_t) read_address (&rnglists_buf, u->addrsize);
	    high = (uintptr_t) read_address (&rnglists_buf, u->addrsize);
	    if (!add_range (state, rdata,
			    libbacktrace_add_base (low, base_address),
			    libbacktrace_add_base (high, base_address),
			    error_callback, data, vec))
	      return 0;
	  }
	  break;

	case DW_RLE_start_length:
	  {
	    uintptr_t low;
	    uintptr_t length;

	    low = (uintptr_t) read_address (&rnglists_buf, u->addrsize);
	    length = (uintptr_t) read_uleb128 (&rnglists_buf);
	    low = libbacktrace_add_base (low, base_address);
	    if (!add_range (state, rdata, low, low + length,
			    error_callback, data, vec))
	      return 0;
	  }
	  break;

	default:
	  dwarf_buf_error (&rnglists_buf, "unrecognized DW_RLE value", -1);
	  return 0;
	}
    }

  if (rnglists_buf.reported_underflow)
    return 0;

  return 1;
}

/* Call ADD_RANGE for each lowpc/highpc pair in PCRANGE.  RDATA is
   passed to ADD_RANGE, and is either a struct unit * or a struct
   function *.  VEC is the vector we are adding ranges to, and is
   either a struct unit_addrs_vector * or a struct function_vector *.
   Returns 1 on success, 0 on error.  */

static int
add_ranges (struct backtrace_state *state,
	    const struct dwarf_sections *dwarf_sections,
	    struct libbacktrace_base_address base_address, int is_bigendian,
	    struct unit *u, uintptr_t base, const struct pcrange *pcrange,
	    int (*add_range) (struct backtrace_state *state, void *rdata,
			      uintptr_t lowpc, uintptr_t highpc,
			      backtrace_error_callback error_callback,
			      void *data, void *vec),
	    void *rdata,
	    backtrace_error_callback error_callback, void *data,
	    void *vec)
{
  if (pcrange->have_lowpc && pcrange->have_highpc)
    return add_low_high_range (state, dwarf_sections, base_address,
			       is_bigendian, u, pcrange, add_range, rdata,
			       error_callback, data, vec);

  if (!pcrange->have_ranges)
    {
      /* Did not find any address ranges to add.  */
      return 1;
    }

  if (u->version < 5)
    return add_ranges_from_ranges (state, dwarf_sections, base_address,
				   is_bigendian, u, base, pcrange, add_range,
				   rdata, error_callback, data, vec);
  else
    return add_ranges_from_rnglists (state, dwarf_sections, base_address,
				     is_bigendian, u, base, pcrange, add_range,
				     rdata, error_callback, data, vec);
}

/* Find the address range covered by a compilation unit, reading from
   UNIT_BUF and adding values to U.  Returns 1 if all data could be
   read, 0 if there is some error.  */

static int
find_address_ranges (struct backtrace_state *state,
		     struct libbacktrace_base_address base_address,
		     struct dwarf_buf *unit_buf,
		     const struct dwarf_sections *dwarf_sections,
		     int is_bigendian, struct dwarf_data *altlink,
		     backtrace_error_callback error_callback, void *data,
		     struct unit *u, struct unit_addrs_vector *addrs,
		     enum dwarf_tag *unit_tag)
{
  while (unit_buf->left > 0)
    {
      uint64_t code;
      const struct abbrev *abbrev;
      struct pcrange pcrange;
      struct attr_val name_val;
      int have_name_val;
      struct attr_val comp_dir_val;
      int have_comp_dir_val;
      size_t i;

      code = read_uleb128 (unit_buf);
      if (code == 0)
	return 1;

      abbrev = lookup_abbrev (&u->abbrevs, code, error_callback, data);
      if (abbrev == NULL)
	return 0;

      if (unit_tag != NULL)
	*unit_tag = abbrev->tag;

      memset (&pcrange, 0, sizeof pcrange);
      memset (&name_val, 0, sizeof name_val);
      have_name_val = 0;
      memset (&comp_dir_val, 0, sizeof comp_dir_val);
      have_comp_dir_val = 0;
      for (i = 0; i < abbrev->num_attrs; ++i)
	{
	  struct attr_val val;

	  if (!read_attribute (abbrev->attrs[i].form, abbrev->attrs[i].val,
			       unit_buf, u->is_dwarf64, u->version,
			       u->addrsize, dwarf_sections, altlink, &val))
	    return 0;

	  switch (abbrev->attrs[i].name)
	    {
	    case DW_AT_low_pc: case DW_AT_high_pc: case DW_AT_ranges:
	      update_pcrange (&abbrev->attrs[i], &val, &pcrange);
	      break;

	    case DW_AT_stmt_list:
	      if ((abbrev->tag == DW_TAG_compile_unit
		   || abbrev->tag == DW_TAG_skeleton_unit)
		  && (val.encoding == ATTR_VAL_UINT
		      || val.encoding == ATTR_VAL_REF_SECTION))
		u->lineoff = val.u.uint;
	      break;

	    case DW_AT_name:
	      if (abbrev->tag == DW_TAG_compile_unit
		  || abbrev->tag == DW_TAG_skeleton_unit)
		{
		  name_val = val;
		  have_name_val = 1;
		}
	      break;

	    case DW_AT_comp_dir:
	      if (abbrev->tag == DW_TAG_compile_unit
		  || abbrev->tag == DW_TAG_skeleton_unit)
		{
		  comp_dir_val = val;
		  have_comp_dir_val = 1;
		}
	      break;

	    case DW_AT_str_offsets_base:
	      if ((abbrev->tag == DW_TAG_compile_unit
		   || abbrev->tag == DW_TAG_skeleton_unit)
		  && val.encoding == ATTR_VAL_REF_SECTION)
		u->str_offsets_base = val.u.uint;
	      break;

	    case DW_AT_addr_base:
	      if ((abbrev->tag == DW_TAG_compile_unit
		   || abbrev->tag == DW_TAG_skeleton_unit)
		  && val.encoding == ATTR_VAL_REF_SECTION)
		u->addr_base = val.u.uint;
	      break;

	    case DW_AT_rnglists_base:
	      if ((abbrev->tag == DW_TAG_compile_unit
		   || abbrev->tag == DW_TAG_skeleton_unit)
		  && val.encoding == ATTR_VAL_REF_SECTION)
		u->rnglists_base = val.u.uint;
	      break;

	    default:
	      break;
	    }
	}

      // Resolve strings after we're sure that we have seen
      // DW_AT_str_offsets_base.
      if (have_name_val)
	{
	  if (!resolve_string (dwarf_sections, u->is_dwarf64, is_bigendian,
			       u->str_offsets_base, &name_val,
			       error_callback, data, &u->filename))
	    return 0;
	}
      if (have_comp_dir_val)
	{
	  if (!resolve_string (dwarf_sections, u->is_dwarf64, is_bigendian,
			       u->str_offsets_base, &comp_dir_val,
			       error_callback, data, &u->comp_dir))
	    return 0;
	}

      if (abbrev->tag == DW_TAG_compile_unit
	  || abbrev->tag == DW_TAG_subprogram
	  || abbrev->tag == DW_TAG_skeleton_unit)
	{
	  if (!add_ranges (state, dwarf_sections, base_address,
			   is_bigendian, u, pcrange.lowpc, &pcrange,
			   add_unit_addr, (void *) u, error_callback, data,
			   (void *) addrs))
	    return 0;

	  /* If we found the PC range in the DW_TAG_compile_unit or
	     DW_TAG_skeleton_unit, we can stop now.  */
	  if ((abbrev->tag == DW_TAG_compile_unit
	       || abbrev->tag == DW_TAG_skeleton_unit)
	      && (pcrange.have_ranges
		  || (pcrange.have_lowpc && pcrange.have_highpc)))
	    return 1;
	}

      if (abbrev->has_children)
	{
	  if (!find_address_ranges (state, base_address, unit_buf,
				    dwarf_sections, is_bigendian, altlink,
				    error_callback, data, u, addrs, NULL))
	    return 0;
	}
    }

  return 1;
}

/* Build a mapping from address ranges to the compilation units where
   the line number information for that range can be found.  Returns 1
   on success, 0 on failure.  */

static int
build_address_map (struct backtrace_state *state,
		   struct libbacktrace_base_address base_address,
		   const struct dwarf_sections *dwarf_sections,
		   int is_bigendian, struct dwarf_data *altlink,
		   backtrace_error_callback error_callback, void *data,
		   struct unit_addrs_vector *addrs,
		   struct unit_vector *unit_vec)
{
  struct dwarf_buf info;
  struct backtrace_vector units;
  size_t units_count;
  size_t i;
  struct unit **pu;
  size_t unit_offset = 0;
  struct unit_addrs *pa;

  memset (&addrs->vec, 0, sizeof addrs->vec);
  memset (&unit_vec->vec, 0, sizeof unit_vec->vec);
  addrs->count = 0;
  unit_vec->count = 0;

  /* Read through the .debug_info section.  FIXME: Should we use the
     .debug_aranges section?  gdb and addr2line don't use it, but I'm
     not sure why.  */

  info.name = ".debug_info";
  info.start = dwarf_sections->data[DEBUG_INFO];
  info.buf = info.start;
  info.left = dwarf_sections->size[DEBUG_INFO];
  info.is_bigendian = is_bigendian;
  info.error_callback = error_callback;
  info.data = data;
  info.reported_underflow = 0;

  memset (&units, 0, sizeof units);
  units_count = 0;

  while (info.left > 0)
    {
      const unsigned char *unit_data_start;
      uint64_t len;
      int is_dwarf64;
      struct dwarf_buf unit_buf;
      int version;
      int unit_type;
      uint64_t abbrev_offset;
      int addrsize;
      struct unit *u;
      enum dwarf_tag unit_tag;

      if (info.reported_underflow)
	goto fail;

      unit_data_start = info.buf;

      len = read_initial_length (&info, &is_dwarf64);
      unit_buf = info;
      unit_buf.left = len;

      if (!advance (&info, len))
	goto fail;

      version = read_uint16 (&unit_buf);
      if (version < 2 || version > 5)
	{
	  dwarf_buf_error (&unit_buf, "unrecognized DWARF version", -1);
	  goto fail;
	}

      if (version < 5)
	unit_type = 0;
      else
	{
	  unit_type = read_byte (&unit_buf);
	  if (unit_type == DW_UT_type || unit_type == DW_UT_split_type)
	    {
	      /* This unit doesn't have anything we need.  */
	      continue;
	    }
	}

      pu = ((struct unit **)
	    backtrace_vector_grow (state, sizeof (struct unit *),
				   error_callback, data, &units));
      if (pu == NULL)
	  goto fail;

      u = ((struct unit *)
	   backtrace_alloc (state, sizeof *u, error_callback, data));
      if (u == NULL)
	goto fail;

      *pu = u;
      ++units_count;

      if (version < 5)
	addrsize = 0; /* Set below.  */
      else
	addrsize = read_byte (&unit_buf);

      memset (&u->abbrevs, 0, sizeof u->abbrevs);
      abbrev_offset = read_offset (&unit_buf, is_dwarf64);
      if (!read_abbrevs (state, abbrev_offset,
			 dwarf_sections->data[DEBUG_ABBREV],
			 dwarf_sections->size[DEBUG_ABBREV],
			 is_bigendian, error_callback, data, &u->abbrevs))
	goto fail;

      if (version < 5)
	addrsize = read_byte (&unit_buf);

      switch (unit_type)
	{
	case 0:
	  break;
	case DW_UT_compile: case DW_UT_partial:
	  break;
	case DW_UT_skeleton: case DW_UT_split_compile:
	  read_uint64 (&unit_buf); /* dwo_id */
	  break;
	default:
	  break;
	}

      u->low_offset = unit_offset;
      unit_offset += len + (is_dwarf64 ? 12 : 4);
      u->high_offset = unit_offset;
      u->unit_data = unit_buf.buf;
      u->unit_data_len = unit_buf.left;
      u->unit_data_offset = unit_buf.buf - unit_data_start;
      u->version = version;
      u->is_dwarf64 = is_dwarf64;
      u->addrsize = addrsize;
      u->filename = NULL;
      u->comp_dir = NULL;
      u->abs_filename = NULL;
      u->lineoff = 0;
      u->str_offsets_base = 0;
      u->addr_base = 0;
      u->rnglists_base = 0;

      /* The actual line number mappings will be read as needed.  */
      u->lines = NULL;
      u->lines_count = 0;
      u->function_addrs = NULL;
      u->function_addrs_count = 0;

      if (!find_address_ranges (state, base_address, &unit_buf, dwarf_sections,
				is_bigendian, altlink, error_callback, data,
				u, addrs, &unit_tag))
	goto fail;

      if (unit_buf.reported_underflow)
	goto fail;
    }
  if (info.reported_underflow)
    goto fail;

  /* Add a trailing addrs entry, but don't include it in addrs->count.  */
  pa = ((struct unit_addrs *)
	backtrace_vector_grow (state, sizeof (struct unit_addrs),
			       error_callback, data, &addrs->vec));
  if (pa == NULL)
    goto fail;
  pa->low = 0;
  --pa->low;
  pa->high = pa->low;
  pa->u = NULL;

  unit_vec->vec = units;
  unit_vec->count = units_count;
  return 1;

 fail:
  if (units_count > 0)
    {
      pu = (struct unit **) units.base;
      for (i = 0; i < units_count; i++)
	{
	  free_abbrevs (state, &pu[i]->abbrevs, error_callback, data);
	  backtrace_free (state, pu[i], sizeof **pu, error_callback, data);
	}
      backtrace_vector_free (state, &units, error_callback, data);
    }
  if (addrs->count > 0)
    {
      backtrace_vector_free (state, &addrs->vec, error_callback, data);
      addrs->count = 0;
    }
  return 0;
}

/* Add a new mapping to the vector of line mappings that we are
   building.  Returns 1 on success, 0 on failure.  */

static int
add_line (struct backtrace_state *state, struct dwarf_data *ddata,
	  uintptr_t pc, const char *filename, int lineno,
	  backtrace_error_callback error_callback, void *data,
	  struct line_vector *vec)
{
  struct line *ln;

  /* If we are adding the same mapping, ignore it.  This can happen
     when using discriminators.  */
  if (vec->count > 0)
    {
      ln = (struct line *) vec->vec.base + (vec->count - 1);
      if (pc == ln->pc && filename == ln->filename && lineno == ln->lineno)
	return 1;
    }

  ln = ((struct line *)
	backtrace_vector_grow (state, sizeof (struct line), error_callback,
			       data, &vec->vec));
  if (ln == NULL)
    return 0;

  /* Add in the base address here, so that we can look up the PC
     directly.  */
  ln->pc = libbacktrace_add_base (pc, ddata->base_address);

  ln->filename = filename;
  ln->lineno = lineno;
  ln->idx = vec->count;

  ++vec->count;

  return 1;
}

/* Free the line header information.  */

static void
free_line_header (struct backtrace_state *state, struct line_header *hdr,
		  backtrace_error_callback error_callback, void *data)
{
  if (hdr->dirs_count != 0)
    backtrace_free (state, hdr->dirs, hdr->dirs_count * sizeof (const char *),
		    error_callback, data);
  backtrace_free (state, hdr->filenames,
		  hdr->filenames_count * sizeof (char *),
		  error_callback, data);
}

/* Read the directories and file names for a line header for version
   2, setting fields in HDR.  Return 1 on success, 0 on failure.  */

static int
read_v2_paths (struct backtrace_state *state, struct unit *u,
	       struct dwarf_buf *hdr_buf, struct line_header *hdr)
{
  const unsigned char *p;
  const unsigned char *pend;
  size_t i;

  /* Count the number of directory entries.  */
  hdr->dirs_count = 0;
  p = hdr_buf->buf;
  pend = p + hdr_buf->left;
  while (p < pend && *p != '\0')
    {
      p += strnlen((const char *) p, pend - p) + 1;
      ++hdr->dirs_count;
    }

  /* The index of the first entry in the list of directories is 1.  Index 0 is
     used for the current directory of the compilation.  To simplify index
     handling, we set entry 0 to the compilation unit directory.  */
  ++hdr->dirs_count;
  hdr->dirs = ((const char **)
	       backtrace_alloc (state,
				hdr->dirs_count * sizeof (const char *),
				hdr_buf->error_callback,
				hdr_buf->data));
  if (hdr->dirs == NULL)
    return 0;

  hdr->dirs[0] = u->comp_dir;
  i = 1;
  while (*hdr_buf->buf != '\0')
    {
      if (hdr_buf->reported_underflow)
	return 0;

      hdr->dirs[i] = read_string (hdr_buf);
      if (hdr->dirs[i] == NULL)
	return 0;
      ++i;
    }
  if (!advance (hdr_buf, 1))
    return 0;

  /* Count the number of file entries.  */
  hdr->filenames_count = 0;
  p = hdr_buf->buf;
  pend = p + hdr_buf->left;
  while (p < pend && *p != '\0')
    {
      p += strnlen ((const char *) p, pend - p) + 1;
      p += leb128_len (p);
      p += leb128_len (p);
      p += leb128_len (p);
      ++hdr->filenames_count;
    }

  /* The index of the first entry in the list of file names is 1.  Index 0 is
     used for the DW_AT_name of the compilation unit.  To simplify index
     handling, we set entry 0 to the compilation unit file name.  */
  ++hdr->filenames_count;
  hdr->filenames = ((const char **)
		    backtrace_alloc (state,
				     hdr->filenames_count * sizeof (char *),
				     hdr_buf->error_callback,
				     hdr_buf->data));
  if (hdr->filenames == NULL)
    return 0;
  hdr->filenames[0] = u->filename;
  i = 1;
  while (*hdr_buf->buf != '\0')
    {
      const char *filename;
      uint64_t dir_index;

      if (hdr_buf->reported_underflow)
	return 0;

      filename = read_string (hdr_buf);
      if (filename == NULL)
	return 0;
      dir_index = read_uleb128 (hdr_buf);
      if (IS_ABSOLUTE_PATH (filename)
	  || (dir_index < hdr->dirs_count && hdr->dirs[dir_index] == NULL))
	hdr->filenames[i] = filename;
      else
	{
	  const char *dir;
	  size_t dir_len;
	  size_t filename_len;
	  char *s;

	  if (dir_index < hdr->dirs_count)
	    dir = hdr->dirs[dir_index];
	  else
	    {
	      dwarf_buf_error (hdr_buf,
			       ("invalid directory index in "
				"line number program header"),
			       0);
	      return 0;
	    }
	  dir_len = strlen (dir);
	  filename_len = strlen (filename);
	  s = ((char *) backtrace_alloc (state, dir_len + filename_len + 2,
					 hdr_buf->error_callback,
					 hdr_buf->data));
	  if (s == NULL)
	    return 0;
	  memcpy (s, dir, dir_len);
	  /* FIXME: If we are on a DOS-based file system, and the
	     directory or the file name use backslashes, then we
	     should use a backslash here.  */
	  s[dir_len] = '/';
	  memcpy (s + dir_len + 1, filename, filename_len + 1);
	  hdr->filenames[i] = s;
	}

      /* Ignore the modification time and size.  */
      read_uleb128 (hdr_buf);
      read_uleb128 (hdr_buf);

      ++i;
    }

  return 1;
}

/* Read a single version 5 LNCT entry for a directory or file name in a
   line header.  Sets *STRING to the resulting name, ignoring other
   data.  Return 1 on success, 0 on failure.  */

static int
read_lnct (struct backtrace_state *state, struct dwarf_data *ddata,
	   struct unit *u, struct dwarf_buf *hdr_buf,
	   const struct line_header *hdr, size_t formats_count,
	   const struct line_header_format *formats, const char **string)
{
  size_t i;
  const char *dir;
  const char *path;

  dir = NULL;
  path = NULL;
  for (i = 0; i < formats_count; i++)
    {
      struct attr_val val;

      if (!read_attribute (formats[i].form, 0, hdr_buf, u->is_dwarf64,
			   u->version, hdr->addrsize, &ddata->dwarf_sections,
			   ddata->altlink, &val))
	return 0;
      switch (formats[i].lnct)
	{
	case DW_LNCT_path:
	  if (!resolve_string (&ddata->dwarf_sections, u->is_dwarf64,
			       ddata->is_bigendian, u->str_offsets_base,
			       &val, hdr_buf->error_callback, hdr_buf->data,
			       &path))
	    return 0;
	  break;
	case DW_LNCT_directory_index:
	  if (val.encoding == ATTR_VAL_UINT)
	    {
	      if (val.u.uint >= hdr->dirs_count)
		{
		  dwarf_buf_error (hdr_buf,
				   ("invalid directory index in "
				    "line number program header"),
				   0);
		  return 0;
		}
	      dir = hdr->dirs[val.u.uint];
	    }
	  break;
	default:
	  /* We don't care about timestamps or sizes or hashes.  */
	  break;
	}
    }

  if (path == NULL)
    {
      dwarf_buf_error (hdr_buf,
		       "missing file name in line number program header",
		       0);
      return 0;
    }

  if (dir == NULL)
    *string = path;
  else
    {
      size_t dir_len;
      size_t path_len;
      char *s;

      dir_len = strlen (dir);
      path_len = strlen (path);
      s = (char *) backtrace_alloc (state, dir_len + path_len + 2,
				    hdr_buf->error_callback, hdr_buf->data);
      if (s == NULL)
	return 0;
      memcpy (s, dir, dir_len);
      /* FIXME: If we are on a DOS-based file system, and the
	 directory or the path name use backslashes, then we should
	 use a backslash here.  */
      s[dir_len] = '/';
      memcpy (s + dir_len + 1, path, path_len + 1);
      *string = s;
    }

  return 1;
}

/* Read a set of DWARF 5 line header format entries, setting *PCOUNT
   and *PPATHS.  Return 1 on success, 0 on failure.  */

static int
read_line_header_format_entries (struct backtrace_state *state,
				 struct dwarf_data *ddata,
				 struct unit *u,
				 struct dwarf_buf *hdr_buf,
				 struct line_header *hdr,
				 size_t *pcount,
				 const char ***ppaths)
{
  size_t formats_count;
  struct line_header_format *formats;
  size_t paths_count;
  const char **paths;
  size_t i;
  int ret;

  formats_count = read_byte (hdr_buf);
  if (formats_count == 0)
    formats = NULL;
  else
    {
      formats = ((struct line_header_format *)
		 backtrace_alloc (state,
				  (formats_count
				   * sizeof (struct line_header_format)),
				  hdr_buf->error_callback,
				  hdr_buf->data));
      if (formats == NULL)
	return 0;

      for (i = 0; i < formats_count; i++)
	{
	  formats[i].lnct = (int) read_uleb128(hdr_buf);
	  formats[i].form = (enum dwarf_form) read_uleb128 (hdr_buf);
	}
    }

  paths_count = read_uleb128 (hdr_buf);
  if (paths_count == 0)
    {
      *pcount = 0;
      *ppaths = NULL;
      ret = 1;
      goto exit;
    }

  paths = ((const char **)
	   backtrace_alloc (state, paths_count * sizeof (const char *),
			    hdr_buf->error_callback, hdr_buf->data));
  if (paths == NULL)
    {
      ret = 0;
      goto exit;
    }
  for (i = 0; i < paths_count; i++)
    {
      if (!read_lnct (state, ddata, u, hdr_buf, hdr, formats_count,
		      formats, &paths[i]))
	{
	  backtrace_free (state, paths,
			  paths_count * sizeof (const char *),
			  hdr_buf->error_callback, hdr_buf->data);
	  ret = 0;
	  goto exit;
	}
    }

  *pcount = paths_count;
  *ppaths = paths;

  ret = 1;

 exit:
  if (formats != NULL)
    backtrace_free (state, formats,
		    formats_count * sizeof (struct line_header_format),
		    hdr_buf->error_callback, hdr_buf->data);

  return  ret;
}

/* Read the line header.  Return 1 on success, 0 on failure.  */

static int
read_line_header (struct backtrace_state *state, struct dwarf_data *ddata,
		  struct unit *u, int is_dwarf64, struct dwarf_buf *line_buf,
		  struct line_header *hdr)
{
  uint64_t hdrlen;
  struct dwarf_buf hdr_buf;

  hdr->version = read_uint16 (line_buf);
  if (hdr->version < 2 || hdr->version > 5)
    {
      dwarf_buf_error (line_buf, "unsupported line number version", -1);
      return 0;
    }

  if (hdr->version < 5)
    hdr->addrsize = u->addrsize;
  else
    {
      hdr->addrsize = read_byte (line_buf);
      /* We could support a non-zero segment_selector_size but I doubt
	 we'll ever see it.  */
      if (read_byte (line_buf) != 0)
	{
	  dwarf_buf_error (line_buf,
			   "non-zero segment_selector_size not supported",
			   -1);
	  return 0;
	}
    }

  hdrlen = read_offset (line_buf, is_dwarf64);

  hdr_buf = *line_buf;
  hdr_buf.left = hdrlen;

  if (!advance (line_buf, hdrlen))
    return 0;

  hdr->min_insn_len = read_byte (&hdr_buf);
  if (hdr->version < 4)
    hdr->max_ops_per_insn = 1;
  else
    hdr->max_ops_per_insn = read_byte (&hdr_buf);

  /* We don't care about default_is_stmt.  */
  read_byte (&hdr_buf);

  hdr->line_base = read_sbyte (&hdr_buf);
  hdr->line_range = read_byte (&hdr_buf);

  hdr->opcode_base = read_byte (&hdr_buf);
  hdr->opcode_lengths = hdr_buf.buf;
  if (!advance (&hdr_buf, hdr->opcode_base - 1))
    return 0;

  if (hdr->version < 5)
    {
      if (!read_v2_paths (state, u, &hdr_buf, hdr))
	return 0;
    }
  else
    {
      if (!read_line_header_format_entries (state, ddata, u, &hdr_buf, hdr,
					    &hdr->dirs_count,
					    &hdr->dirs))
	return 0;
      if (!read_line_header_format_entries (state, ddata, u, &hdr_buf, hdr,
					    &hdr->filenames_count,
					    &hdr->filenames))
	return 0;
    }

  if (hdr_buf.reported_underflow)
    return 0;

  return 1;
}

/* Read the line program, adding line mappings to VEC.  Return 1 on
   success, 0 on failure.  */

static int
read_line_program (struct backtrace_state *state, struct dwarf_data *ddata,
		   const struct line_header *hdr, struct dwarf_buf *line_buf,
		   struct line_vector *vec)
{
  uint64_t address;
  unsigned int op_index;
  const char *reset_filename;
  const char *filename;
  int lineno;

  address = 0;
  op_index = 0;
  if (hdr->filenames_count > 1)
    reset_filename = hdr->filenames[1];
  else
    reset_filename = "";
  filename = reset_filename;
  lineno = 1;
  while (line_buf->left > 0)
    {
      unsigned int op;

      op = read_byte (line_buf);
      if (op >= hdr->opcode_base)
	{
	  unsigned int advance;

	  /* Special opcode.  */
	  op -= hdr->opcode_base;
	  advance = op / hdr->line_range;
	  address += (hdr->min_insn_len * (op_index + advance)
		      / hdr->max_ops_per_insn);
	  op_index = (op_index + advance) % hdr->max_ops_per_insn;
	  lineno += hdr->line_base + (int) (op % hdr->line_range);
	  add_line (state, ddata, address, filename, lineno,
		    line_buf->error_callback, line_buf->data, vec);
	}
      else if (op == DW_LNS_extended_op)
	{
	  uint64_t len;

	  len = read_uleb128 (line_buf);
	  op = read_byte (line_buf);
	  switch (op)
	    {
	    case DW_LNE_end_sequence:
	      /* FIXME: Should we mark the high PC here?  It seems
		 that we already have that information from the
		 compilation unit.  */
	      address = 0;
	      op_index = 0;
	      filename = reset_filename;
	      lineno = 1;
	      break;
	    case DW_LNE_set_address:
	      address = read_address (line_buf, hdr->addrsize);
	      break;
	    case DW_LNE_define_file:
	      {
		const char *f;
		unsigned int dir_index;

		f = read_string (line_buf);
		if (f == NULL)
		  return 0;
		dir_index = read_uleb128 (line_buf);
		/* Ignore that time and length.  */
		read_uleb128 (line_buf);
		read_uleb128 (line_buf);
		if (IS_ABSOLUTE_PATH (f))
		  filename = f;
		else
		  {
		    const char *dir;
		    size_t dir_len;
		    size_t f_len;
		    char *p;

		    if (dir_index < hdr->dirs_count)
		      dir = hdr->dirs[dir_index];
		    else
		      {
			dwarf_buf_error (line_buf,
					 ("invalid directory index "
					  "in line number program"),
					 0);
			return 0;
		      }
		    dir_len = strlen (dir);
		    f_len = strlen (f);
		    p = ((char *)
			 backtrace_alloc (state, dir_len + f_len + 2,
					  line_buf->error_callback,
					  line_buf->data));
		    if (p == NULL)
		      return 0;
		    memcpy (p, dir, dir_len);
		    /* FIXME: If we are on a DOS-based file system,
		       and the directory or the file name use
		       backslashes, then we should use a backslash
		       here.  */
		    p[dir_len] = '/';
		    memcpy (p + dir_len + 1, f, f_len + 1);
		    filename = p;
		  }
	      }
	      break;
	    case DW_LNE_set_discriminator:
	      /* We don't care about discriminators.  */
	      read_uleb128 (line_buf);
	      break;
	    default:
	      if (!advance (line_buf, len - 1))
		return 0;
	      break;
	    }
	}
      else
	{
	  switch (op)
	    {
	    case DW_LNS_copy:
	      add_line (state, ddata, address, filename, lineno,
			line_buf->error_callback, line_buf->data, vec);
	      break;
	    case DW_LNS_advance_pc:
	      {
		uint64_t advance;

		advance = read_uleb128 (line_buf);
		address += (hdr->min_insn_len * (op_index + advance)
			    / hdr->max_ops_per_insn);
		op_index = (op_index + advance) % hdr->max_ops_per_insn;
	      }
	      break;
	    case DW_LNS_advance_line:
	      lineno += (int) read_sleb128 (line_buf);
	      break;
	    case DW_LNS_set_file:
	      {
		uint64_t fileno;

		fileno = read_uleb128 (line_buf);
		if (fileno >= hdr->filenames_count)
		  {
		    dwarf_buf_error (line_buf,
				     ("invalid file number in "
				      "line number program"),
				     0);
		    return 0;
		  }
		filename = hdr->filenames[fileno];
	      }
	      break;
	    case DW_LNS_set_column:
	      read_uleb128 (line_buf);
	      break;
	    case DW_LNS_negate_stmt:
	      break;
	    case DW_LNS_set_basic_block:
	      break;
	    case DW_LNS_const_add_pc:
	      {
		unsigned int advance;

		op = 255 - hdr->opcode_base;
		advance = op / hdr->line_range;
		address += (hdr->min_insn_len * (op_index + advance)
			    / hdr->max_ops_per_insn);
		op_index = (op_index + advance) % hdr->max_ops_per_insn;
	      }
	      break;
	    case DW_LNS_fixed_advance_pc:
	      address += read_uint16 (line_buf);
	      op_index = 0;
	      break;
	    case DW_LNS_set_prologue_end:
	      break;
	    case DW_LNS_set_epilogue_begin:
	      break;
	    case DW_LNS_set_isa:
	      read_uleb128 (line_buf);
	      break;
	    default:
	      {
		unsigned int i;

		for (i = hdr->opcode_lengths[op - 1]; i > 0; --i)
		  read_uleb128 (line_buf);
	      }
	      break;
	    }
	}
    }

  return 1;
}

/* Read the line number information for a compilation unit.  Returns 1
   on success, 0 on failure.  */

static int
read_line_info (struct backtrace_state *state, struct dwarf_data *ddata,
		backtrace_error_callback error_callback, void *data,
		struct unit *u, struct line_header *hdr, struct line **lines,
		size_t *lines_count)
{
  struct line_vector vec;
  struct dwarf_buf line_buf;
  uint64_t len;
  int is_dwarf64;
  struct line *ln;

  memset (&vec.vec, 0, sizeof vec.vec);
  vec.count = 0;

  memset (hdr, 0, sizeof *hdr);

  if (u->lineoff != (off_t) (size_t) u->lineoff
      || (size_t) u->lineoff >= ddata->dwarf_sections.size[DEBUG_LINE])
    {
      error_callback (data, "unit line offset out of range", 0);
      goto fail;
    }

  line_buf.name = ".debug_line";
  line_buf.start = ddata->dwarf_sections.data[DEBUG_LINE];
  line_buf.buf = ddata->dwarf_sections.data[DEBUG_LINE] + u->lineoff;
  line_buf.left = ddata->dwarf_sections.size[DEBUG_LINE] - u->lineoff;
  line_buf.is_bigendian = ddata->is_bigendian;
  line_buf.error_callback = error_callback;
  line_buf.data = data;
  line_buf.reported_underflow = 0;

  len = read_initial_length (&line_buf, &is_dwarf64);
  line_buf.left = len;

  if (!read_line_header (state, ddata, u, is_dwarf64, &line_buf, hdr))
    goto fail;

  if (!read_line_program (state, ddata, hdr, &line_buf, &vec))
    goto fail;

  if (line_buf.reported_underflow)
    goto fail;

  if (vec.count == 0)
    {
      /* This is not a failure in the sense of generating an error,
	 but it is a failure in that sense that we have no useful
	 information.  */
      goto fail;
    }

  /* Allocate one extra entry at the end.  */
  ln = ((struct line *)
	backtrace_vector_grow (state, sizeof (struct line), error_callback,
			       data, &vec.vec));
  if (ln == NULL)
    goto fail;
  ln->pc = (uintptr_t) -1;
  ln->filename = NULL;
  ln->lineno = 0;
  ln->idx = 0;

  if (!backtrace_vector_release (state, &vec.vec, error_callback, data))
    goto fail;

  ln = (struct line *) vec.vec.base;
  backtrace_qsort (ln, vec.count, sizeof (struct line), line_compare);

  *lines = ln;
  *lines_count = vec.count;

  return 1;

 fail:
  backtrace_vector_free (state, &vec.vec, error_callback, data);
  free_line_header (state, hdr, error_callback, data);
  *lines = (struct line *) (uintptr_t) -1;
  *lines_count = 0;
  return 0;
}

static const char *read_referenced_name (struct dwarf_data *, struct unit *,
					 uint64_t, backtrace_error_callback,
					 void *);

/* Read the name of a function from a DIE referenced by ATTR with VAL.  */

static const char *
read_referenced_name_from_attr (struct dwarf_data *ddata, struct unit *u,
				struct attr *attr, struct attr_val *val,
				backtrace_error_callback error_callback,
				void *data)
{
  switch (attr->name)
    {
    case DW_AT_abstract_origin:
    case DW_AT_specification:
      break;
    default:
      return NULL;
    }

  if (attr->form == DW_FORM_ref_sig8)
    return NULL;

  if (val->encoding == ATTR_VAL_REF_INFO)
    {
      struct unit *unit
	= find_unit (ddata->units, ddata->units_count,
		     val->u.uint);
      if (unit == NULL)
	return NULL;

      uint64_t offset = val->u.uint - unit->low_offset;
      return read_referenced_name (ddata, unit, offset, error_callback, data);
    }

  if (val->encoding == ATTR_VAL_UINT
      || val->encoding == ATTR_VAL_REF_UNIT)
    return read_referenced_name (ddata, u, val->u.uint, error_callback, data);

  if (val->encoding == ATTR_VAL_REF_ALT_INFO)
    {
      struct unit *alt_unit
	= find_unit (ddata->altlink->units, ddata->altlink->units_count,
		     val->u.uint);
      if (alt_unit == NULL)
	return NULL;

      uint64_t offset = val->u.uint - alt_unit->low_offset;
      return read_referenced_name (ddata->altlink, alt_unit, offset,
				   error_callback, data);
    }

  return NULL;
}

/* Read the name of a function from a DIE referenced by a
   DW_AT_abstract_origin or DW_AT_specification tag.  OFFSET is within
   the same compilation unit.  */

static const char *
read_referenced_name (struct dwarf_data *ddata, struct unit *u,
		      uint64_t offset, backtrace_error_callback error_callback,
		      void *data)
{
  struct dwarf_buf unit_buf;
  uint64_t code;
  const struct abbrev *abbrev;
  const char *ret;
  size_t i;

  /* OFFSET is from the start of the data for this compilation unit.
     U->unit_data is the data, but it starts U->unit_data_offset bytes
     from the beginning.  */

  if (offset < u->unit_data_offset
      || offset - u->unit_data_offset >= u->unit_data_len)
    {
      error_callback (data,
		      "abstract origin or specification out of range",
		      0);
      return NULL;
    }

  offset -= u->unit_data_offset;

  unit_buf.name = ".debug_info";
  unit_buf.start = ddata->dwarf_sections.data[DEBUG_INFO];
  unit_buf.buf = u->unit_data + offset;
  unit_buf.left = u->unit_data_len - offset;
  unit_buf.is_bigendian = ddata->is_bigendian;
  unit_buf.error_callback = error_callback;
  unit_buf.data = data;
  unit_buf.reported_underflow = 0;

  code = read_uleb128 (&unit_buf);
  if (code == 0)
    {
      dwarf_buf_error (&unit_buf,
		       "invalid abstract origin or specification",
		       0);
      return NULL;
    }

  abbrev = lookup_abbrev (&u->abbrevs, code, error_callback, data);
  if (abbrev == NULL)
    return NULL;

  ret = NULL;
  for (i = 0; i < abbrev->num_attrs; ++i)
    {
      struct attr_val val;

      if (!read_attribute (abbrev->attrs[i].form, abbrev->attrs[i].val,
			   &unit_buf, u->is_dwarf64, u->version, u->addrsize,
			   &ddata->dwarf_sections, ddata->altlink, &val))
	return NULL;

      switch (abbrev->attrs[i].name)
	{
	case DW_AT_name:
	  /* Third name preference: don't override.  A name we found in some
	     other way, will normally be more useful -- e.g., this name is
	     normally not mangled.  */
	  if (ret != NULL)
	    break;
	  if (!resolve_string (&ddata->dwarf_sections, u->is_dwarf64,
			       ddata->is_bigendian, u->str_offsets_base,
			       &val, error_callback, data, &ret))
	    return NULL;
	  break;

	case DW_AT_linkage_name:
	case DW_AT_MIPS_linkage_name:
	  /* First name preference: override all.  */
	  {
	    const char *s;

	    s = NULL;
	    if (!resolve_string (&ddata->dwarf_sections, u->is_dwarf64,
				 ddata->is_bigendian, u->str_offsets_base,
				 &val, error_callback, data, &s))
	      return NULL;
	    if (s != NULL)
	      return s;
	  }
	  break;

	case DW_AT_specification:
	  /* Second name preference: override DW_AT_name, don't override
	     DW_AT_linkage_name.  */
	  {
	    const char *name;

	    name = read_referenced_name_from_attr (ddata, u, &abbrev->attrs[i],
						   &val, error_callback, data);
	    if (name != NULL)
	      ret = name;
	  }
	  break;

	default:
	  break;
	}
    }

  return ret;
}

/* Add a range to a unit that maps to a function.  This is called via
   add_ranges.  Returns 1 on success, 0 on error.  */

static int
add_function_range (struct backtrace_state *state, void *rdata,
		    uintptr_t lowpc, uintptr_t highpc,
		    backtrace_error_callback error_callback, void *data,
		    void *pvec)
{
  struct function *function = (struct function *) rdata;
  struct function_vector *vec = (struct function_vector *) pvec;
  struct function_addrs *p;

  if (vec->count > 0)
    {
      p = (struct function_addrs *) vec->vec.base + (vec->count - 1);
      if ((lowpc == p->high || lowpc == p->high + 1)
	  && function == p->function)
	{
	  if (highpc > p->high)
	    p->high = highpc;
	  return 1;
	}
    }

  p = ((struct function_addrs *)
       backtrace_vector_grow (state, sizeof (struct function_addrs),
			      error_callback, data, &vec->vec));
  if (p == NULL)
    return 0;

  p->low = lowpc;
  p->high = highpc;
  p->function = function;

  ++vec->count;

  return 1;
}

/* Read one entry plus all its children.  Add function addresses to
   VEC.  Returns 1 on success, 0 on error.  */

static int
read_function_entry (struct backtrace_state *state, struct dwarf_data *ddata,
		     struct unit *u, uintptr_t base, struct dwarf_buf *unit_buf,
		     const struct line_header *lhdr,
		     backtrace_error_callback error_callback, void *data,
		     struct function_vector *vec_function,
		     struct function_vector *vec_inlined)
{
  while (unit_buf->left > 0)
    {
      uint64_t code;
      const struct abbrev *abbrev;
      int is_function;
      struct function *function;
      struct function_vector *vec;
      size_t i;
      struct pcrange pcrange;
      int have_linkage_name;

      code = read_uleb128 (unit_buf);
      if (code == 0)
	return 1;

      abbrev = lookup_abbrev (&u->abbrevs, code, error_callback, data);
      if (abbrev == NULL)
	return 0;

      is_function = (abbrev->tag == DW_TAG_subprogram
		     || abbrev->tag == DW_TAG_entry_point
		     || abbrev->tag == DW_TAG_inlined_subroutine);

      if (abbrev->tag == DW_TAG_inlined_subroutine)
	vec = vec_inlined;
      else
	vec = vec_function;

      function = NULL;
      if (is_function)
	{
	  function = ((struct function *)
		      backtrace_alloc (state, sizeof *function,
				       error_callback, data));
	  if (function == NULL)
	    return 0;
	  memset (function, 0, sizeof *function);
	}

      memset (&pcrange, 0, sizeof pcrange);
      have_linkage_name = 0;
      for (i = 0; i < abbrev->num_attrs; ++i)
	{
	  struct attr_val val;

	  if (!read_attribute (abbrev->attrs[i].form, abbrev->attrs[i].val,
			       unit_buf, u->is_dwarf64, u->version,
			       u->addrsize, &ddata->dwarf_sections,
			       ddata->altlink, &val))
	    return 0;

	  /* The compile unit sets the base address for any address
	     ranges in the function entries.  */
	  if ((abbrev->tag == DW_TAG_compile_unit
	       || abbrev->tag == DW_TAG_skeleton_unit)
	      && abbrev->attrs[i].name == DW_AT_low_pc)
	    {
	      if (val.encoding == ATTR_VAL_ADDRESS)
		base = (uintptr_t) val.u.uint;
	      else if (val.encoding == ATTR_VAL_ADDRESS_INDEX)
		{
		  if (!resolve_addr_index (&ddata->dwarf_sections,
					   u->addr_base, u->addrsize,
					   ddata->is_bigendian, val.u.uint,
					   error_callback, data, &base))
		    return 0;
		}
	    }

	  if (is_function)
	    {
	      switch (abbrev->attrs[i].name)
		{
		case DW_AT_call_file:
		  if (val.encoding == ATTR_VAL_UINT)
		    {
		      if (val.u.uint >= lhdr->filenames_count)
			{
			  dwarf_buf_error (unit_buf,
					   ("invalid file number in "
					    "DW_AT_call_file attribute"),
					   0);
			  return 0;
			}
		      function->caller_filename = lhdr->filenames[val.u.uint];
		    }
		  break;

		case DW_AT_call_line:
		  if (val.encoding == ATTR_VAL_UINT)
		    function->caller_lineno = val.u.uint;
		  break;

		case DW_AT_abstract_origin:
		case DW_AT_specification:
		  /* Second name preference: override DW_AT_name, don't override
		     DW_AT_linkage_name.  */
		  if (have_linkage_name)
		    break;
		  {
		    const char *name;

		    name
		      = read_referenced_name_from_attr (ddata, u,
							&abbrev->attrs[i], &val,
							error_callback, data);
		    if (name != NULL)
		      function->name = name;
		  }
		  break;

		case DW_AT_name:
		  /* Third name preference: don't override.  */
		  if (function->name != NULL)
		    break;
		  if (!resolve_string (&ddata->dwarf_sections, u->is_dwarf64,
				       ddata->is_bigendian,
				       u->str_offsets_base, &val,
				       error_callback, data, &function->name))
		    return 0;
		  break;

		case DW_AT_linkage_name:
		case DW_AT_MIPS_linkage_name:
		  /* First name preference: override all.  */
		  {
		    const char *s;

		    s = NULL;
		    if (!resolve_string (&ddata->dwarf_sections, u->is_dwarf64,
					 ddata->is_bigendian,
					 u->str_offsets_base, &val,
					 error_callback, data, &s))
		      return 0;
		    if (s != NULL)
		      {
			function->name = s;
			have_linkage_name = 1;
		      }
		  }
		  break;

		case DW_AT_low_pc: case DW_AT_high_pc: case DW_AT_ranges:
		  update_pcrange (&abbrev->attrs[i], &val, &pcrange);
		  break;

		default:
		  break;
		}
	    }
	}

      /* If we couldn't find a name for the function, we have no use
	 for it.  */
      if (is_function && function->name == NULL)
	{
	  backtrace_free (state, function, sizeof *function,
			  error_callback, data);
	  is_function = 0;
	}

      if (is_function)
	{
	  if (pcrange.have_ranges
	      || (pcrange.have_lowpc && pcrange.have_highpc))
	    {
	      if (!add_ranges (state, &ddata->dwarf_sections,
			       ddata->base_address, ddata->is_bigendian,
			       u, base, &pcrange, add_function_range,
			       (void *) function, error_callback, data,
			       (void *) vec))
		return 0;
	    }
	  else
	    {
	      backtrace_free (state, function, sizeof *function,
			      error_callback, data);
	      is_function = 0;
	    }
	}

      if (abbrev->has_children)
	{
	  if (!is_function)
	    {
	      if (!read_function_entry (state, ddata, u, base, unit_buf, lhdr,
					error_callback, data, vec_function,
					vec_inlined))
		return 0;
	    }
	  else
	    {
	      struct function_vector fvec;

	      /* Gather any information for inlined functions in
		 FVEC.  */

	      memset (&fvec, 0, sizeof fvec);

	      if (!read_function_entry (state, ddata, u, base, unit_buf, lhdr,
					error_callback, data, vec_function,
					&fvec))
		return 0;

	      if (fvec.count > 0)
		{
		  struct function_addrs *p;
		  struct function_addrs *faddrs;

		  /* Allocate a trailing entry, but don't include it
		     in fvec.count.  */
		  p = ((struct function_addrs *)
		       backtrace_vector_grow (state,
					      sizeof (struct function_addrs),
					      error_callback, data,
					      &fvec.vec));
		  if (p == NULL)
		    return 0;
		  p->low = 0;
		  --p->low;
		  p->high = p->low;
		  p->function = NULL;

		  if (!backtrace_vector_release (state, &fvec.vec,
						 error_callback, data))
		    return 0;

		  faddrs = (struct function_addrs *) fvec.vec.base;
		  backtrace_qsort (faddrs, fvec.count,
				   sizeof (struct function_addrs),
				   function_addrs_compare);

		  function->function_addrs = faddrs;
		  function->function_addrs_count = fvec.count;
		}
	    }
	}
    }

  return 1;
}

/* Read function name information for a compilation unit.  We look
   through the whole unit looking for function tags.  */

static void
read_function_info (struct backtrace_state *state, struct dwarf_data *ddata,
		    const struct line_header *lhdr,
		    backtrace_error_callback error_callback, void *data,
		    struct unit *u, struct function_vector *fvec,
		    struct function_addrs **ret_addrs,
		    size_t *ret_addrs_count)
{
  struct function_vector lvec;
  struct function_vector *pfvec;
  struct dwarf_buf unit_buf;
  struct function_addrs *p;
  struct function_addrs *addrs;
  size_t addrs_count;

  /* Use FVEC if it is not NULL.  Otherwise use our own vector.  */
  if (fvec != NULL)
    pfvec = fvec;
  else
    {
      memset (&lvec, 0, sizeof lvec);
      pfvec = &lvec;
    }

  unit_buf.name = ".debug_info";
  unit_buf.start = ddata->dwarf_sections.data[DEBUG_INFO];
  unit_buf.buf = u->unit_data;
  unit_buf.left = u->unit_data_len;
  unit_buf.is_bigendian = ddata->is_bigendian;
  unit_buf.error_callback = error_callback;
  unit_buf.data = data;
  unit_buf.reported_underflow = 0;

  while (unit_buf.left > 0)
    {
      if (!read_function_entry (state, ddata, u, 0, &unit_buf, lhdr,
				error_callback, data, pfvec, pfvec))
	return;
    }

  if (pfvec->count == 0)
    return;

  /* Allocate a trailing entry, but don't include it in
     pfvec->count.  */
  p = ((struct function_addrs *)
       backtrace_vector_grow (state, sizeof (struct function_addrs),
			      error_callback, data, &pfvec->vec));
  if (p == NULL)
    return;
  p->low = 0;
  --p->low;
  p->high = p->low;
  p->function = NULL;

  addrs_count = pfvec->count;

  if (fvec == NULL)
    {
      if (!backtrace_vector_release (state, &lvec.vec, error_callback, data))
	return;
      addrs = (struct function_addrs *) pfvec->vec.base;
    }
  else
    {
      /* Finish this list of addresses, but leave the remaining space in
	 the vector available for the next function unit.  */
      addrs = ((struct function_addrs *)
	       backtrace_vector_finish (state, &fvec->vec,
					error_callback, data));
      if (addrs == NULL)
	return;
      fvec->count = 0;
    }

  backtrace_qsort (addrs, addrs_count, sizeof (struct function_addrs),
		   function_addrs_compare);

  *ret_addrs = addrs;
  *ret_addrs_count = addrs_count;
}

/* See if PC is inlined in FUNCTION.  If it is, print out the inlined
   information, and update FILENAME and LINENO for the caller.
   Returns whatever CALLBACK returns, or 0 to keep going.  */

static int
report_inlined_functions (uintptr_t pc, struct function *function,
			  backtrace_full_callback callback, void *data,
			  const char **filename, int *lineno)
{
  struct function_addrs *p;
  struct function_addrs *match;
  struct function *inlined;
  int ret;

  if (function->function_addrs_count == 0)
    return 0;

  /* Our search isn't safe if pc == -1, as that is the sentinel
     value.  */
  if (pc + 1 == 0)
    return 0;

  p = ((struct function_addrs *)
       bsearch (&pc, function->function_addrs,
		function->function_addrs_count,
		sizeof (struct function_addrs),
		function_addrs_search));
  if (p == NULL)
    return 0;

  /* Here pc >= p->low && pc < (p + 1)->low.  The function_addrs are
     sorted by low, so if pc > p->low we are at the end of a range of
     function_addrs with the same low value.  If pc == p->low walk
     forward to the end of the range with that low value.  Then walk
     backward and use the first range that includes pc.  */
  while (pc == (p + 1)->low)
    ++p;
  match = NULL;
  while (1)
    {
      if (pc < p->high)
	{
	  match = p;
	  break;
	}
      if (p == function->function_addrs)
	break;
      if ((p - 1)->low < p->low)
	break;
      --p;
    }
  if (match == NULL)
    return 0;

  /* We found an inlined call.  */

  inlined = match->function;

  /* Report any calls inlined into this one.  */
  ret = report_inlined_functions (pc, inlined, callback, data,
				  filename, lineno);
  if (ret != 0)
    return ret;

  /* Report this inlined call.  */
  ret = callback (data, pc, *filename, *lineno, inlined->name);
  if (ret != 0)
    return ret;

  /* Our caller will report the caller of the inlined function; tell
     it the appropriate filename and line number.  */
  *filename = inlined->caller_filename;
  *lineno = inlined->caller_lineno;

  return 0;
}

/* Look for a PC in the DWARF mapping for one module.  On success,
   call CALLBACK and return whatever it returns.  On error, call
   ERROR_CALLBACK and return 0.  Sets *FOUND to 1 if the PC is found,
   0 if not.  */

static int
dwarf_lookup_pc (struct backtrace_state *state, struct dwarf_data *ddata,
		 uintptr_t pc, backtrace_full_callback callback,
		 backtrace_error_callback error_callback, void *data,
		 int *found)
{
  struct unit_addrs *entry;
  int found_entry;
  struct unit *u;
  int new_data;
  struct line *lines;
  struct line *ln;
  struct function_addrs *p;
  struct function_addrs *fmatch;
  struct function *function;
  const char *filename;
  int lineno;
  int ret;

  *found = 1;

  /* Find an address range that includes PC.  Our search isn't safe if
     PC == -1, as we use that as a sentinel value, so skip the search
     in that case.  */
  entry = (ddata->addrs_count == 0 || pc + 1 == 0
	   ? NULL
	   : bsearch (&pc, ddata->addrs, ddata->addrs_count,
		      sizeof (struct unit_addrs), unit_addrs_search));

  if (entry == NULL)
    {
      *found = 0;
      return 0;
    }

  /* Here pc >= entry->low && pc < (entry + 1)->low.  The unit_addrs
     are sorted by low, so if pc > p->low we are at the end of a range
     of unit_addrs with the same low value.  If pc == p->low walk
     forward to the end of the range with that low value.  Then walk
     backward and use the first range that includes pc.  */
  while (pc == (entry + 1)->low)
    ++entry;
  found_entry = 0;
  while (1)
    {
      if (pc < entry->high)
	{
	  found_entry = 1;
	  break;
	}
      if (entry == ddata->addrs)
	break;
      if ((entry - 1)->low < entry->low)
	break;
      --entry;
    }
  if (!found_entry)
    {
      *found = 0;
      return 0;
    }

  /* We need the lines, lines_count, function_addrs,
     function_addrs_count fields of u.  If they are not set, we need
     to set them.  When running in threaded mode, we need to allow for
     the possibility that some other thread is setting them
     simultaneously.  */

  u = entry->u;
  lines = u->lines;

  /* Skip units with no useful line number information by walking
     backward.  Useless line number information is marked by setting
     lines == -1.  */
  while (entry > ddata->addrs
	 && pc >= (entry - 1)->low
	 && pc < (entry - 1)->high)
    {
      if (state->threaded)
	lines = (struct line *) backtrace_atomic_load_pointer (&u->lines);

      if (lines != (struct line *) (uintptr_t) -1)
	break;

      --entry;

      u = entry->u;
      lines = u->lines;
    }

  if (state->threaded)
    lines = backtrace_atomic_load_pointer (&u->lines);

  new_data = 0;
  if (lines == NULL)
    {
      struct function_addrs *function_addrs;
      size_t function_addrs_count;
      struct line_header lhdr;
      size_t count;

      /* We have never read the line information for this unit.  Read
	 it now.  */

      function_addrs = NULL;
      function_addrs_count = 0;
      if (read_line_info (state, ddata, error_callback, data, entry->u, &lhdr,
			  &lines, &count))
	{
	  struct function_vector *pfvec;

	  /* If not threaded, reuse DDATA->FVEC for better memory
	     consumption.  */
	  if (state->threaded)
	    pfvec = NULL;
	  else
	    pfvec = &ddata->fvec;
	  read_function_info (state, ddata, &lhdr, error_callback, data,
			      entry->u, pfvec, &function_addrs,
			      &function_addrs_count);
	  free_line_header (state, &lhdr, error_callback, data);
	  new_data = 1;
	}

      /* Atomically store the information we just read into the unit.
	 If another thread is simultaneously writing, it presumably
	 read the same information, and we don't care which one we
	 wind up with; we just leak the other one.  We do have to
	 write the lines field last, so that the acquire-loads above
	 ensure that the other fields are set.  */

      if (!state->threaded)
	{
	  u->lines_count = count;
	  u->function_addrs = function_addrs;
	  u->function_addrs_count = function_addrs_count;
	  u->lines = lines;
	}
      else
	{
	  backtrace_atomic_store_size_t (&u->lines_count, count);
	  backtrace_atomic_store_pointer (&u->function_addrs, function_addrs);
	  backtrace_atomic_store_size_t (&u->function_addrs_count,
					 function_addrs_count);
	  backtrace_atomic_store_pointer (&u->lines, lines);
	}
    }

  /* Now all fields of U have been initialized.  */

  if (lines == (struct line *) (uintptr_t) -1)
    {
      /* If reading the line number information failed in some way,
	 try again to see if there is a better compilation unit for
	 this PC.  */
      if (new_data)
	return dwarf_lookup_pc (state, ddata, pc, callback, error_callback,
				data, found);
      return callback (data, pc, NULL, 0, NULL);
    }

  /* Search for PC within this unit.  */

  ln = (struct line *) bsearch (&pc, lines, entry->u->lines_count,
				sizeof (struct line), line_search);
  if (ln == NULL)
    {
      /* The PC is between the low_pc and high_pc attributes of the
	 compilation unit, but no entry in the line table covers it.
	 This implies that the start of the compilation unit has no
	 line number information.  */

      if (entry->u->abs_filename == NULL)
	{
	  const char *filename;

	  filename = entry->u->filename;
	  if (filename != NULL
	      && !IS_ABSOLUTE_PATH (filename)
	      && entry->u->comp_dir != NULL)
	    {
	      size_t filename_len;
	      const char *dir;
	      size_t dir_len;
	      char *s;

	      filename_len = strlen (filename);
	      dir = entry->u->comp_dir;
	      dir_len = strlen (dir);
	      s = (char *) backtrace_alloc (state, dir_len + filename_len + 2,
					    error_callback, data);
	      if (s == NULL)
		{
		  *found = 0;
		  return 0;
		}
	      memcpy (s, dir, dir_len);
	      /* FIXME: Should use backslash if DOS file system.  */
	      s[dir_len] = '/';
	      memcpy (s + dir_len + 1, filename, filename_len + 1);
	      filename = s;
	    }
	  entry->u->abs_filename = filename;
	}

      return callback (data, pc, entry->u->abs_filename, 0, NULL);
    }

  /* Search for function name within this unit.  */

  if (entry->u->function_addrs_count == 0)
    return callback (data, pc, ln->filename, ln->lineno, NULL);

  p = ((struct function_addrs *)
       bsearch (&pc, entry->u->function_addrs,
		entry->u->function_addrs_count,
		sizeof (struct function_addrs),
		function_addrs_search));
  if (p == NULL)
    return callback (data, pc, ln->filename, ln->lineno, NULL);

  /* Here pc >= p->low && pc < (p + 1)->low.  The function_addrs are
     sorted by low, so if pc > p->low we are at the end of a range of
     function_addrs with the same low value.  If pc == p->low walk
     forward to the end of the range with that low value.  Then walk
     backward and use the first range that includes pc.  */
  while (pc == (p + 1)->low)
    ++p;
  fmatch = NULL;
  while (1)
    {
      if (pc < p->high)
	{
	  fmatch = p;
	  break;
	}
      if (p == entry->u->function_addrs)
	break;
      if ((p - 1)->low < p->low)
	break;
      --p;
    }
  if (fmatch == NULL)
    return callback (data, pc, ln->filename, ln->lineno, NULL);

  function = fmatch->function;

  filename = ln->filename;
  lineno = ln->lineno;

  ret = report_inlined_functions (pc, function, callback, data,
				  &filename, &lineno);
  if (ret != 0)
    return ret;

  return callback (data, pc, filename, lineno, function->name);
}


/* Return the file/line information for a PC using the DWARF mapping
   we built earlier.  */

static int
dwarf_fileline (struct backtrace_state *state, uintptr_t pc,
		backtrace_full_callback callback,
		backtrace_error_callback error_callback, void *data)
{
  struct dwarf_data *ddata;
  int found;
  int ret;

  if (!state->threaded)
    {
      for (ddata = (struct dwarf_data *) state->fileline_data;
	   ddata != NULL;
	   ddata = ddata->next)
	{
	  ret = dwarf_lookup_pc (state, ddata, pc, callback, error_callback,
				 data, &found);
	  if (ret != 0 || found)
	    return ret;
	}
    }
  else
    {
      struct dwarf_data **pp;

      pp = (struct dwarf_data **) (void *) &state->fileline_data;
      while (1)
	{
	  ddata = backtrace_atomic_load_pointer (pp);
	  if (ddata == NULL)
	    break;

	  ret = dwarf_lookup_pc (state, ddata, pc, callback, error_callback,
				 data, &found);
	  if (ret != 0 || found)
	    return ret;

	  pp = &ddata->next;
	}
    }

  /* FIXME: See if any libraries have been dlopen'ed.  */

  return callback (data, pc, NULL, 0, NULL);
}

/* Initialize our data structures from the DWARF debug info for a
   file.  Return NULL on failure.  */

static struct dwarf_data *
build_dwarf_data (struct backtrace_state *state,
		  struct libbacktrace_base_address base_address,
		  const struct dwarf_sections *dwarf_sections,
		  int is_bigendian,
		  struct dwarf_data *altlink,
		  backtrace_error_callback error_callback,
		  void *data)
{
  struct unit_addrs_vector addrs_vec;
  struct unit_vector units_vec;
  struct dwarf_data *fdata;

  if (!build_address_map (state, base_address, dwarf_sections, is_bigendian,
			  altlink, error_callback, data, &addrs_vec,
			  &units_vec))
    return NULL;

  if (!backtrace_vector_release (state, &addrs_vec.vec, error_callback, data))
    return NULL;
  if (!backtrace_vector_release (state, &units_vec.vec, error_callback, data))
    return NULL;

  backtrace_qsort ((struct unit_addrs *) addrs_vec.vec.base, addrs_vec.count,
		   sizeof (struct unit_addrs), unit_addrs_compare);
  if (!resolve_unit_addrs_overlap (state, error_callback, data, &addrs_vec))
    return NULL;

  /* No qsort for units required, already sorted.  */

  fdata = ((struct dwarf_data *)
	   backtrace_alloc (state, sizeof (struct dwarf_data),
			    error_callback, data));
  if (fdata == NULL)
    return NULL;

  fdata->next = NULL;
  fdata->altlink = altlink;
  fdata->base_address = base_address;
  fdata->addrs = (struct unit_addrs *) addrs_vec.vec.base;
  fdata->addrs_count = addrs_vec.count;
  fdata->units = (struct unit **) units_vec.vec.base;
  fdata->units_count = units_vec.count;
  fdata->dwarf_sections = *dwarf_sections;
  fdata->is_bigendian = is_bigendian;
  memset (&fdata->fvec, 0, sizeof fdata->fvec);

  return fdata;
}

/* Build our data structures from the DWARF sections for a module.
   Set FILELINE_FN and STATE->FILELINE_DATA.  Return 1 on success, 0
   on failure.  */

int
backtrace_dwarf_add (struct backtrace_state *state,
		     struct libbacktrace_base_address base_address,
		     const struct dwarf_sections *dwarf_sections,
		     int is_bigendian,
		     struct dwarf_data *fileline_altlink,
		     backtrace_error_callback error_callback,
		     void *data, fileline *fileline_fn,
		     struct dwarf_data **fileline_entry)
{
  struct dwarf_data *fdata;

  fdata = build_dwarf_data (state, base_address, dwarf_sections, is_bigendian,
			    fileline_altlink, error_callback, data);
  if (fdata == NULL)
    return 0;

  if (fileline_entry != NULL)
    *fileline_entry = fdata;

  if (!state->threaded)
    {
      struct dwarf_data **pp;

      for (pp = (struct dwarf_data **) (void *) &state->fileline_data;
	   *pp != NULL;
	   pp = &(*pp)->next)
	;
      *pp = fdata;
    }
  else
    {
      while (1)
	{
	  struct dwarf_data **pp;

	  pp = (struct dwarf_data **) (void *) &state->fileline_data;

	  while (1)
	    {
	      struct dwarf_data *p;

	      p = backtrace_atomic_load_pointer (pp);

	      if (p == NULL)
		break;

	      pp = &p->next;
	    }

	  if (__sync_bool_compare_and_swap (pp, NULL, fdata))
	    break;
	}
    }

  *fileline_fn = dwarf_fileline;

  return 1;
}
// fileline.c:
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#if defined (HAVE_KERN_PROC_ARGS) || defined (HAVE_KERN_PROC)
#include <sys/sysctl.h>
#endif

#ifdef HAVE_MACH_O_DYLD_H
#include <mach-o/dyld.h>
#endif

#ifdef __hpux__
#include <dl.h>
#endif

#ifdef HAVE_WINDOWS_H
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#endif


#ifndef HAVE_GETEXECNAME
#define getexecname() NULL
#endif

#ifdef __hpux__
static char *
hpux_get_executable_path (struct backtrace_state *state,
			  backtrace_error_callback error_callback, void *data)
{
  struct shl_descriptor *desc;
  size_t len = sizeof (struct shl_descriptor);

  desc = backtrace_alloc (state, len, error_callback, data);
  if (desc == NULL)
    return NULL;

  if (shl_get_r (0, desc) == -1)
    {
      backtrace_free (state, desc, len, error_callback, data);
      return NULL;
    }

  return desc->filename;
}

#else

#define hpux_get_executable_path(state, error_callback, data) NULL

#endif

#if !defined (HAVE_KERN_PROC_ARGS) && !defined (HAVE_KERN_PROC)

#define sysctl_exec_name1(state, error_callback, data) NULL
#define sysctl_exec_name2(state, error_callback, data) NULL

#else /* defined (HAVE_KERN_PROC_ARGS) || |defined (HAVE_KERN_PROC) */

static char *
sysctl_exec_name (struct backtrace_state *state,
		  int mib0, int mib1, int mib2, int mib3,
		  backtrace_error_callback error_callback, void *data)
{
  int mib[4];
  size_t len;
  char *name;
  size_t rlen;

  mib[0] = mib0;
  mib[1] = mib1;
  mib[2] = mib2;
  mib[3] = mib3;

  if (sysctl (mib, 4, NULL, &len, NULL, 0) < 0)
    return NULL;
  name = (char *) backtrace_alloc (state, len, error_callback, data);
  if (name == NULL)
    return NULL;
  rlen = len;
  if (sysctl (mib, 4, name, &rlen, NULL, 0) < 0)
    {
      backtrace_free (state, name, len, error_callback, data);
      return NULL;
    }
  return name;
}

#ifdef HAVE_KERN_PROC_ARGS

static char *
sysctl_exec_name1 (struct backtrace_state *state,
		   backtrace_error_callback error_callback, void *data)
{
  /* This variant is used on NetBSD.  */
  return sysctl_exec_name (state, CTL_KERN, KERN_PROC_ARGS, -1,
			   KERN_PROC_PATHNAME, error_callback, data);
}

#else

#define sysctl_exec_name1(state, error_callback, data) NULL

#endif

#ifdef HAVE_KERN_PROC

static char *
sysctl_exec_name2 (struct backtrace_state *state,
		   backtrace_error_callback error_callback, void *data)
{
  /* This variant is used on FreeBSD.  */
  return sysctl_exec_name (state, CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1,
			   error_callback, data);
}

#else

#define sysctl_exec_name2(state, error_callback, data) NULL

#endif

#endif /* defined (HAVE_KERN_PROC_ARGS) || |defined (HAVE_KERN_PROC) */

#ifdef HAVE_MACH_O_DYLD_H

static char *
macho_get_executable_path (struct backtrace_state *state,
			   backtrace_error_callback error_callback, void *data)
{
  uint32_t len;
  char *name;

  len = 0;
  if (_NSGetExecutablePath (NULL, &len) == 0)
    return NULL;
  name = (char *) backtrace_alloc (state, len, error_callback, data);
  if (name == NULL)
    return NULL;
  if (_NSGetExecutablePath (name, &len) != 0)
    {
      backtrace_free (state, name, len, error_callback, data);
      return NULL;
    }
  return name;
}

#else /* !defined (HAVE_MACH_O_DYLD_H) */

#define macho_get_executable_path(state, error_callback, data) NULL

#endif /* !defined (HAVE_MACH_O_DYLD_H) */

#if HAVE_DECL__PGMPTR

#define windows_executable_filename() _pgmptr

#else /* !HAVE_DECL__PGMPTR */

#define windows_executable_filename() NULL

#endif /* !HAVE_DECL__PGMPTR */

#ifdef HAVE_WINDOWS_H

#define FILENAME_BUF_SIZE (MAX_PATH)

static char *
windows_get_executable_path (char *buf, backtrace_error_callback error_callback,
			     void *data)
{
  size_t got;
  int error;

  got = GetModuleFileNameA (NULL, buf, FILENAME_BUF_SIZE - 1);
  error = GetLastError ();
  if (got == 0
      || (got == FILENAME_BUF_SIZE - 1 && error == ERROR_INSUFFICIENT_BUFFER))
    {
      error_callback (data,
		      "could not get the filename of the current executable",
		      error);
      return NULL;
    }
  return buf;
}

#else /* !defined (HAVE_WINDOWS_H) */

#define windows_get_executable_path(buf, error_callback, data) NULL
#define FILENAME_BUF_SIZE 64

#endif /* !defined (HAVE_WINDOWS_H) */

/* Initialize the fileline information from the executable.  Returns 1
   on success, 0 on failure.  */

static int
fileline_initialize (struct backtrace_state *state,
		     backtrace_error_callback error_callback, void *data)
{
  int failed;
  fileline fileline_fn;
  int pass;
  int called_error_callback;
  int descriptor;
  const char *filename;
  char buf[FILENAME_BUF_SIZE];

  if (!state->threaded)
    failed = state->fileline_initialization_failed;
  else
    failed = backtrace_atomic_load_int (&state->fileline_initialization_failed);

  if (failed)
    {
      error_callback (data, "failed to read executable information", -1);
      return 0;
    }

  if (!state->threaded)
    fileline_fn = state->fileline_fn;
  else
    fileline_fn = backtrace_atomic_load_pointer (&state->fileline_fn);
  if (fileline_fn != NULL)
    return 1;

  /* We have not initialized the information.  Do it now.  */

  descriptor = -1;
  called_error_callback = 0;
  for (pass = 0; pass < 11; ++pass)
    {
      int does_not_exist;

      switch (pass)
	{
	case 0:
	  filename = state->filename;
	  break;
	case 1:
	  filename = getexecname ();
	  break;
	case 2:
	  /* Test this before /proc/self/exe, as the latter exists but points
	     to the wine binary (and thus doesn't work).  */
	  filename = windows_executable_filename ();
	  break;
	case 3:
	  filename = "/proc/self/exe";
	  break;
	case 4:
	  filename = "/proc/curproc/file";
	  break;
	case 5:
	  snprintf (buf, sizeof (buf), "/proc/%ld/object/a.out",
		    (long) getpid ());
	  filename = buf;
	  break;
	case 6:
	  filename = sysctl_exec_name1 (state, error_callback, data);
	  break;
	case 7:
	  filename = sysctl_exec_name2 (state, error_callback, data);
	  break;
	case 8:
	  filename = macho_get_executable_path (state, error_callback, data);
	  break;
	case 9:
	  filename = windows_get_executable_path (buf, error_callback, data);
	  break;
	case 10:
	  filename = hpux_get_executable_path (state, error_callback, data);
	  break;
	default:
	  abort ();
	}

      if (filename == NULL)
	continue;

      descriptor = backtrace_open (filename, error_callback, data,
				   &does_not_exist);
      if (descriptor < 0 && !does_not_exist)
	{
	  called_error_callback = 1;
	  break;
	}
      if (descriptor >= 0)
	break;
    }

  if (descriptor < 0)
    {
      if (!called_error_callback)
	{
	  if (state->filename != NULL)
	    error_callback (data, state->filename, ENOENT);
	  else
	    error_callback (data,
			    "libbacktrace could not find executable to open",
			    0);
	}
      failed = 1;
    }

  if (!failed)
    {
      if (!backtrace_initialize (state, filename, descriptor, error_callback,
				 data, &fileline_fn))
	failed = 1;
    }

  if (failed)
    {
      if (!state->threaded)
	state->fileline_initialization_failed = 1;
      else
	backtrace_atomic_store_int (&state->fileline_initialization_failed, 1);
      return 0;
    }

  if (!state->threaded)
    state->fileline_fn = fileline_fn;
  else
    {
      backtrace_atomic_store_pointer (&state->fileline_fn, fileline_fn);

      /* Note that if two threads initialize at once, one of the data
	 sets may be leaked.  */
    }

  return 1;
}

/* Given a PC, find the file name, line number, and function name.  */

int
backtrace_pcinfo (struct backtrace_state *state, uintptr_t pc,
		  backtrace_full_callback callback,
		  backtrace_error_callback error_callback, void *data)
{
  if (!fileline_initialize (state, error_callback, data))
    return 0;

  if (state->fileline_initialization_failed)
    return 0;

  return state->fileline_fn (state, pc, callback, error_callback, data);
}

/* Given a PC, find the symbol for it, and its value.  */

int
backtrace_syminfo (struct backtrace_state *state, uintptr_t pc,
		   backtrace_syminfo_callback callback,
		   backtrace_error_callback error_callback, void *data)
{
  if (!fileline_initialize (state, error_callback, data))
    return 0;

  if (state->fileline_initialization_failed)
    return 0;

  state->syminfo_fn (state, pc, callback, error_callback, data);
  return 1;
}

/* A backtrace_syminfo_callback that can call into a
   backtrace_full_callback, used when we have a symbol table but no
   debug info.  */

void
backtrace_syminfo_to_full_callback (void *data, uintptr_t pc,
				    const char *symname,
				    uintptr_t symval ATTRIBUTE_UNUSED,
				    uintptr_t symsize ATTRIBUTE_UNUSED)
{
  struct backtrace_call_full *bdata = (struct backtrace_call_full *) data;

  bdata->ret = bdata->full_callback (bdata->full_data, pc, NULL, 0, symname);
}

/* An error callback that corresponds to
   backtrace_syminfo_to_full_callback.  */

void
backtrace_syminfo_to_full_error_callback (void *data, const char *msg,
					  int errnum)
{
  struct backtrace_call_full *bdata = (struct backtrace_call_full *) data;

  bdata->full_error_callback (bdata->full_data, msg, errnum);
}
// posix.c:
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif

#ifndef FD_CLOEXEC
#define FD_CLOEXEC 1
#endif

/* Open a file for reading.  */

int
backtrace_open (const char *filename, backtrace_error_callback error_callback,
		void *data, int *does_not_exist)
{
  int descriptor;

  if (does_not_exist != NULL)
    *does_not_exist = 0;

  descriptor = open (filename, (int) (O_RDONLY | O_BINARY | O_CLOEXEC));
  if (descriptor < 0)
    {
      /* If DOES_NOT_EXIST is not NULL, then don't call ERROR_CALLBACK
	 if the file does not exist.  We treat lacking permission to
	 open the file as the file not existing; this case arises when
	 running the libgo syscall package tests as root.  */
      if (does_not_exist != NULL && (errno == ENOENT || errno == EACCES))
	*does_not_exist = 1;
      else
	error_callback (data, filename, errno);
      return -1;
    }

#ifdef HAVE_FCNTL
  /* Set FD_CLOEXEC just in case the kernel does not support
     O_CLOEXEC. It doesn't matter if this fails for some reason.
     FIXME: At some point it should be safe to only do this if
     O_CLOEXEC == 0.  */
  fcntl (descriptor, F_SETFD, FD_CLOEXEC);
#endif

  return descriptor;
}

/* Close DESCRIPTOR.  */

int
backtrace_close (int descriptor, backtrace_error_callback error_callback,
		 void *data)
{
  if (close (descriptor) < 0)
    {
      error_callback (data, "close", errno);
      return 0;
    }
  return 1;
}
// print.c:
#include <stdio.h>
#include <string.h>
#include <sys/types.h>


/* Passed to callbacks.  */

struct print_data
{
  struct backtrace_state *state;
  FILE *f;
};

/* Print errors to stderr.  */

static void
error_callback (void *data, const char *msg, int errnum)
{
  struct print_data *pdata = (struct print_data *) data;

  if (pdata->state->filename != NULL)
    fprintf (stderr, "%s: ", pdata->state->filename);
  fprintf (stderr, "libbacktrace: %s", msg);
  if (errnum > 0)
    fprintf (stderr, ": %s", strerror (errnum));
  fputc ('\n', stderr);
}

/* Print one level of a backtrace if we couldn't get a file or function name.
   Use syminfo to try to get a symbol name.  */

static void print_syminfo_callback (void *data, uintptr_t pc,
				    const char *symname, uintptr_t symval,
				    uintptr_t symsize ATTRIBUTE_UNUSED)
{
  struct print_data *pdata = (struct print_data *) data;

  if (symname == NULL)
    fprintf (pdata->f, "0x%lx ???\n\t???:0\n", (unsigned long) pc);
  else
    fprintf (pdata->f, "0x%lx ???\n\t%s+0x%lx:0\n",
	     (unsigned long) pc,
	     symname,
	     (unsigned long) (pc - symval));
}

/* Print one level of a backtrace.  */

static int
print_callback (void *data, uintptr_t pc, const char *filename, int lineno,
		const char *function)
{
  struct print_data *pdata = (struct print_data *) data;

  if (function == NULL && filename == NULL)
    {
      backtrace_syminfo (pdata->state, pc, print_syminfo_callback,
			 error_callback, data);
      return 0;
    }

  fprintf (pdata->f, "0x%lx %s\n\t%s:%d\n",
	   (unsigned long) pc,
	   function == NULL ? "???" : function,
	   filename == NULL ? "???" : filename,
	   lineno);
  return 0;
}

/* Print a backtrace.  */

void __attribute__((noinline))
backtrace_print (struct backtrace_state *state, int skip, FILE *f)
{
  struct print_data data;

  data.state = state;
  data.f = f;
  backtrace_full (state, skip + 1, print_callback, error_callback,
		  (void *) &data);
}
// sort.c:
#include <stddef.h>
#include <sys/types.h>


/* The GNU glibc version of qsort allocates memory, which we must not
   do if we are invoked by a signal handler.  So provide our own
   sort.  */

static void
swap (char *a, char *b, size_t size)
{
  size_t i;

  for (i = 0; i < size; i++, a++, b++)
    {
      char t;

      t = *a;
      *a = *b;
      *b = t;
    }
}

void
backtrace_qsort (void *basearg, size_t count, size_t size,
		 int (*compar) (const void *, const void *))
{
  char *base = (char *) basearg;
  size_t i;
  size_t mid;

 tail_recurse:
  if (count < 2)
    return;

  /* The symbol table and DWARF tables, which is all we use this
     routine for, tend to be roughly sorted.  Pick the middle element
     in the array as our pivot point, so that we are more likely to
     cut the array in half for each recursion step.  */
  swap (base, base + (count / 2) * size, size);

  mid = 0;
  for (i = 1; i < count; i++)
    {
      if ((*compar) (base, base + i * size) > 0)
	{
	  ++mid;
	  if (i != mid)
	    swap (base + mid * size, base + i * size, size);
	}
    }

  if (mid > 0)
    swap (base, base + mid * size, size);

  /* Recurse with the smaller array, loop with the larger one.  That
     ensures that our maximum stack depth is log count.  */
  if (2 * mid < count)
    {
      backtrace_qsort (base, mid, size, compar);
      base += (mid + 1) * size;
      count -= mid + 1;
      goto tail_recurse;
    }
  else
    {
      backtrace_qsort (base + (mid + 1) * size, count - (mid + 1),
		       size, compar);
      count = mid;
      goto tail_recurse;
    }
}
// state.c:
#include <string.h>
#include <sys/types.h>


/* Create the backtrace state.  This will then be passed to all the
   other routines.  */

struct backtrace_state *
backtrace_create_state (const char *filename, int threaded,
			backtrace_error_callback error_callback,
			void *data)
{
  struct backtrace_state init_state;
  struct backtrace_state *state;

#ifndef HAVE_SYNC_FUNCTIONS
  if (threaded)
    {
      error_callback (data, "backtrace library does not support threads", 0);
      return NULL;
    }
#endif

  memset (&init_state, 0, sizeof init_state);
  init_state.filename = filename;
  init_state.threaded = threaded;

  state = ((struct backtrace_state *)
	   backtrace_alloc (&init_state, sizeof *state, error_callback, data));
  if (state == NULL)
    return NULL;
  *state = init_state;

  return state;
}
// backtrace.c:
#include <sys/types.h>

#include <unwind.h>

/* The main backtrace_full routine.  */

/* Data passed through _Unwind_Backtrace.  */

struct backtrace_data
{
  /* Number of frames to skip.  */
  int skip;
  /* Library state.  */
  struct backtrace_state *state;
  /* Callback routine.  */
  backtrace_full_callback callback;
  /* Error callback routine.  */
  backtrace_error_callback error_callback;
  /* Data to pass to callback routines.  */
  void *data;
  /* Value to return from backtrace_full.  */
  int ret;
  /* Whether there is any memory available.  */
  int can_alloc;
};

/* Unwind library callback routine.  This is passed to
   _Unwind_Backtrace.  */

static _Unwind_Reason_Code
unwind (struct _Unwind_Context *context, void *vdata)
{
  struct backtrace_data *bdata = (struct backtrace_data *) vdata;
  uintptr_t pc;
  int ip_before_insn = 0;

#ifdef HAVE_GETIPINFO
  pc = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  pc = _Unwind_GetIP (context);
#endif

  if (bdata->skip > 0)
    {
      --bdata->skip;
      return _URC_NO_REASON;
    }

  if (!ip_before_insn)
    --pc;

  if (!bdata->can_alloc)
    bdata->ret = bdata->callback (bdata->data, pc, NULL, 0, NULL);
  else
    bdata->ret = backtrace_pcinfo (bdata->state, pc, bdata->callback,
				   bdata->error_callback, bdata->data);
  if (bdata->ret != 0)
    return _URC_END_OF_STACK;

  return _URC_NO_REASON;
}

/* Get a stack backtrace.  */

int __attribute__((noinline))
backtrace_full (struct backtrace_state *state, int skip,
		backtrace_full_callback callback,
		backtrace_error_callback error_callback, void *data)
{
  struct backtrace_data bdata;
  void *p;

  bdata.skip = skip + 1;
  bdata.state = state;
  bdata.callback = callback;
  bdata.error_callback = error_callback;
  bdata.data = data;
  bdata.ret = 0;

  /* If we can't allocate any memory at all, don't try to produce
     file/line information.  */
  p = backtrace_alloc (state, 4096, NULL, NULL);
  if (p == NULL)
    bdata.can_alloc = 0;
  else
    {
      backtrace_free (state, p, 4096, NULL, NULL);
      bdata.can_alloc = 1;
    }

  _Unwind_Backtrace (unwind, &bdata);
  return bdata.ret;
}
// simple.c:
#include <unwind.h>

/* The simple_backtrace routine.  */

/* Data passed through _Unwind_Backtrace.  */

struct backtrace_simple_data
{
  /* Number of frames to skip.  */
  int skip;
  /* Library state.  */
  struct backtrace_state *state;
  /* Callback routine.  */
  backtrace_simple_callback callback;
  /* Error callback routine.  */
  backtrace_error_callback error_callback;
  /* Data to pass to callback routine.  */
  void *data;
  /* Value to return from backtrace.  */
  int ret;
};

/* Unwind library callback routine.  This is passed to
   _Unwind_Backtrace.  */

static _Unwind_Reason_Code
simple_unwind (struct _Unwind_Context *context, void *vdata)
{
  struct backtrace_simple_data *bdata = (struct backtrace_simple_data *) vdata;
  uintptr_t pc;
  int ip_before_insn = 0;

#ifdef HAVE_GETIPINFO
  pc = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  pc = _Unwind_GetIP (context);
#endif

  if (bdata->skip > 0)
    {
      --bdata->skip;
      return _URC_NO_REASON;
    }

  if (!ip_before_insn)
    --pc;

  bdata->ret = bdata->callback (bdata->data, pc);

  if (bdata->ret != 0)
    return _URC_END_OF_STACK;

  return _URC_NO_REASON;
}

/* Get a simple stack backtrace.  */

int __attribute__((noinline))
backtrace_simple (struct backtrace_state *state, int skip,
		  backtrace_simple_callback callback,
		  backtrace_error_callback error_callback, void *data)
{
  struct backtrace_simple_data bdata;

  bdata.skip = skip + 1;
  bdata.state = state;
  bdata.callback = callback;
  bdata.error_callback = error_callback;
  bdata.data = data;
  bdata.ret = 0;
  _Unwind_Backtrace (simple_unwind, &bdata);
  return bdata.ret;
}
