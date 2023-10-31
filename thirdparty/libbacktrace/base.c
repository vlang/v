// NOTE: Portions of the code have been modified in order to fix compilation in TCC - Ned

// backtrace.h:
#ifndef BACKTRACE_H
#define BACKTRACE_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

struct backtrace_state;

typedef void (*backtrace_error_callback)(void *data, const char *msg,
                                         int errnum);

extern struct backtrace_state *backtrace_create_state(
    const char *filename, int threaded, backtrace_error_callback error_callback,
    void *data);

typedef int (*backtrace_full_callback)(void *data, uintptr_t pc,
                                       const char *filename, int lineno,
                                       const char *function);

extern int backtrace_full(struct backtrace_state *state, int skip,
                          backtrace_full_callback callback,
                          backtrace_error_callback error_callback, void *data);

typedef int (*backtrace_simple_callback)(void *data, uintptr_t pc);

extern int backtrace_simple(struct backtrace_state *state, int skip,
                            backtrace_simple_callback callback,
                            backtrace_error_callback error_callback,
                            void *data);

extern void backtrace_print(struct backtrace_state *state, int skip, FILE *);

extern int backtrace_pcinfo(struct backtrace_state *state, uintptr_t pc,
                            backtrace_full_callback callback,
                            backtrace_error_callback error_callback,
                            void *data);

typedef void (*backtrace_syminfo_callback)(void *data, uintptr_t pc,
                                           const char *symname,
                                           uintptr_t symval, uintptr_t symsize);

extern int backtrace_syminfo(struct backtrace_state *state, uintptr_t addr,
                             backtrace_syminfo_callback callback,
                             backtrace_error_callback error_callback,
                             void *data);

#ifdef __cplusplus
}
#endif

#endif

// internal.h:
#ifndef BACKTRACE_INTERNAL_H
#define BACKTRACE_INTERNAL_H

#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif

#if (GCC_VERSION < 2007)
#define __attribute__(x)
#endif

#ifndef ATTRIBUTE_UNUSED
#define ATTRIBUTE_UNUSED __attribute__((__unused__))
#endif

#ifndef ATTRIBUTE_MALLOC
#if (GCC_VERSION >= 2096)
#define ATTRIBUTE_MALLOC __attribute__((__malloc__))
#else
#define ATTRIBUTE_MALLOC
#endif
#endif

#ifndef ATTRIBUTE_FALLTHROUGH
#if (GCC_VERSION >= 7000)
#define ATTRIBUTE_FALLTHROUGH __attribute__((__fallthrough__))
#else
#define ATTRIBUTE_FALLTHROUGH
#endif
#endif

#ifndef HAVE_SYNC_FUNCTIONS

#define __sync_bool_compare_and_swap(A, B, C) (abort(), 1)
#define __sync_lock_test_and_set(A, B) (abort(), 0)
#define __sync_lock_release(A) abort()

#endif
#ifdef HAVE_ATOMIC_FUNCTIONS

#define backtrace_atomic_load_pointer(p) __atomic_load_n((p), __ATOMIC_ACQUIRE)
#define backtrace_atomic_load_int(p) __atomic_load_n((p), __ATOMIC_ACQUIRE)
#define backtrace_atomic_store_pointer(p, v) \
  __atomic_store_n((p), (v), __ATOMIC_RELEASE)
#define backtrace_atomic_store_size_t(p, v) \
  __atomic_store_n((p), (v), __ATOMIC_RELEASE)
#define backtrace_atomic_store_int(p, v) \
  __atomic_store_n((p), (v), __ATOMIC_RELEASE)

#else
#ifdef HAVE_SYNC_FUNCTIONS

extern void *backtrace_atomic_load_pointer(void *);
extern int backtrace_atomic_load_int(int *);
extern void backtrace_atomic_store_pointer(void *, void *);
extern void backtrace_atomic_store_size_t(size_t *, size_t);
extern void backtrace_atomic_store_int(int *, int);

#else

#define backtrace_atomic_load_pointer(p) (abort(), (void *)NULL)
#define backtrace_atomic_load_int(p) (abort(), 0)
#define backtrace_atomic_store_pointer(p, v) abort()
#define backtrace_atomic_store_size_t(p, v) abort()
#define backtrace_atomic_store_int(p, v) abort()

#endif
#endif

typedef int (*fileline)(struct backtrace_state *state, uintptr_t pc,
                        backtrace_full_callback callback,
                        backtrace_error_callback error_callback, void *data);

typedef void (*syminfo)(struct backtrace_state *state, uintptr_t pc,
                        backtrace_syminfo_callback callback,
                        backtrace_error_callback error_callback, void *data);

struct backtrace_state {
  const char *filename;

  int threaded;

  void *lock;

  fileline fileline_fn;

  void *fileline_data;

  syminfo syminfo_fn;

  void *syminfo_data;

  int fileline_initialization_failed;

  int lock_alloc;

  struct backtrace_freelist_struct *freelist;
};

extern int backtrace_open(const char *filename,
                          backtrace_error_callback error_callback, void *data,
                          int *does_not_exist);

struct backtrace_view {
  const void *data;

  void *base;

  size_t len;
};

extern int backtrace_get_view(struct backtrace_state *state, int descriptor,
                              off_t offset, uint64_t size,
                              backtrace_error_callback error_callback,
                              void *data, struct backtrace_view *view);

extern void backtrace_release_view(struct backtrace_state *state,
                                   struct backtrace_view *view,
                                   backtrace_error_callback error_callback,
                                   void *data);

extern int backtrace_close(int descriptor,
                           backtrace_error_callback error_callback, void *data);

extern void backtrace_qsort(void *base, size_t count, size_t size,
                            int (*compar)(const void *, const void *));

extern void *backtrace_alloc(struct backtrace_state *state, size_t size,
                             backtrace_error_callback error_callback,
                             void *data) ATTRIBUTE_MALLOC;

extern void backtrace_free(struct backtrace_state *state, void *mem,
                           size_t size, backtrace_error_callback error_callback,
                           void *data);

struct backtrace_vector {
  void *base;

  size_t size;

  size_t alc;
};

extern void *backtrace_vector_grow(struct backtrace_state *state, size_t size,
                                   backtrace_error_callback error_callback,
                                   void *data, struct backtrace_vector *vec);

extern void *backtrace_vector_finish(struct backtrace_state *state,
                                     struct backtrace_vector *vec,
                                     backtrace_error_callback error_callback,
                                     void *data);

extern int backtrace_vector_release(struct backtrace_state *state,
                                    struct backtrace_vector *vec,
                                    backtrace_error_callback error_callback,
                                    void *data);

static inline void backtrace_vector_free(
    struct backtrace_state *state, struct backtrace_vector *vec,
    backtrace_error_callback error_callback, void *data) {
  vec->alc += vec->size;
  vec->size = 0;
  backtrace_vector_release(state, vec, error_callback, data);
}

extern int backtrace_initialize(struct backtrace_state *state,
                                const char *filename, int descriptor,
                                backtrace_error_callback error_callback,
                                void *data, fileline *fileline_fn);

enum dwarf_section {
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

struct dwarf_sections {
  const unsigned char *data[DEBUG_MAX];
  size_t size[DEBUG_MAX];
};

struct dwarf_data;

extern int backtrace_dwarf_add(struct backtrace_state *state,
                               uintptr_t base_address,
                               const struct dwarf_sections *dwarf_sections,
                               int is_bigendian,
                               struct dwarf_data *fileline_altlink,
                               backtrace_error_callback error_callback,
                               void *data, fileline *fileline_fn,
                               struct dwarf_data **fileline_entry);

struct backtrace_call_full {
  backtrace_full_callback full_callback;
  backtrace_error_callback full_error_callback;
  void *full_data;
  int ret;
};

extern void backtrace_syminfo_to_full_callback(void *data, uintptr_t pc,
                                               const char *symname,
                                               uintptr_t symval,
                                               uintptr_t symsize);

extern void backtrace_syminfo_to_full_error_callback(void *, const char *, int);

extern int backtrace_uncompress_zdebug(struct backtrace_state *,
                                       const unsigned char *compressed,
                                       size_t compressed_size,
                                       backtrace_error_callback, void *data,
                                       unsigned char **uncompressed,
                                       size_t *uncompressed_size);

extern int backtrace_uncompress_lzma(struct backtrace_state *,
                                     const unsigned char *compressed,
                                     size_t compressed_size,
                                     backtrace_error_callback, void *data,
                                     unsigned char **uncompressed,
                                     size_t *uncompressed_size);

#endif

// filenames.h:
#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif

#if (GCC_VERSION < 2007)
#define __attribute__(x)
#endif

#ifndef ATTRIBUTE_UNUSED
#define ATTRIBUTE_UNUSED __attribute__((__unused__))
#endif

#if defined(__MSDOS__) || defined(_WIN32) || defined(__OS2__) || \
    defined(__CYGWIN__)
#define IS_DIR_SEPARATOR(c) ((c) == '/' || (c) == '\\')
#define HAS_DRIVE_SPEC(f) ((f)[0] != '\0' && (f)[1] == ':')
#define IS_ABSOLUTE_PATH(f) (IS_DIR_SEPARATOR((f)[0]) || HAS_DRIVE_SPEC(f))
#else
#define IS_DIR_SEPARATOR(c) ((c) == '/')
#define IS_ABSOLUTE_PATH(f) (IS_DIR_SEPARATOR((f)[0]))
#endif

// atomic.c:
#include <sys/types.h>

// dwarf.c:
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

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

static size_t xstrnlen(const char *s, size_t maxlen) {
  size_t i;

  for (i = 0; i < maxlen; ++i)
    if (s[i] == '\0') break;
  return i;
}

#define strnlen xstrnlen

#endif

struct dwarf_buf {
  const char *name;

  const unsigned char *start;

  const unsigned char *buf;

  size_t left;

  int is_bigendian;

  backtrace_error_callback error_callback;

  void *data;

  int reported_underflow;
};

struct attr {
  enum dwarf_attribute name;

  enum dwarf_form form;

  int64_t val;
};

struct abbrev {
  uint64_t code;

  enum dwarf_tag tag;

  int has_children;

  size_t num_attrs;

  struct attr *attrs;
};

struct abbrevs {
  size_t num_abbrevs;

  struct abbrev *abbrevs;
};

enum attr_val_encoding {

  ATTR_VAL_NONE,

  ATTR_VAL_ADDRESS,

  ATTR_VAL_ADDRESS_INDEX,

  ATTR_VAL_UINT,

  ATTR_VAL_SINT,

  ATTR_VAL_STRING,

  ATTR_VAL_STRING_INDEX,

  ATTR_VAL_REF_UNIT,

  ATTR_VAL_REF_INFO,

  ATTR_VAL_REF_ALT_INFO,

  ATTR_VAL_REF_SECTION,

  ATTR_VAL_REF_TYPE,

  ATTR_VAL_RNGLISTS_INDEX,

  ATTR_VAL_BLOCK,

  ATTR_VAL_EXPR,
};

struct attr_val {
  enum attr_val_encoding encoding;
  union {
    uint64_t uint;

    int64_t sint;

    const char *string;

  } u;
};

struct line_header {
  int version;

  int addrsize;

  unsigned int min_insn_len;

  unsigned int max_ops_per_insn;

  int line_base;

  unsigned int line_range;

  unsigned int opcode_base;

  const unsigned char *opcode_lengths;

  size_t dirs_count;

  const char **dirs;

  size_t filenames_count;

  const char **filenames;
};

struct line_header_format {
  int lnct;
  enum dwarf_form form;
};

struct line {
  uintptr_t pc;

  const char *filename;

  int lineno;

  int idx;
};

struct line_vector {
  struct backtrace_vector vec;

  size_t count;
};

struct function {
  const char *name;

  const char *caller_filename;

  int caller_lineno;

  struct function_addrs *function_addrs;
  size_t function_addrs_count;
};

struct function_addrs {
  uint64_t low;
  uint64_t high;

  struct function *function;
};

struct function_vector {
  struct backtrace_vector vec;

  size_t count;
};

struct unit {
  const unsigned char *unit_data;

  size_t unit_data_len;

  size_t unit_data_offset;

  size_t low_offset;

  size_t high_offset;

  int version;

  int is_dwarf64;

  int addrsize;

  off_t lineoff;

  uint64_t str_offsets_base;

  uint64_t addr_base;

  uint64_t rnglists_base;

  const char *filename;

  const char *comp_dir;

  const char *abs_filename;

  struct abbrevs abbrevs;

  struct line *lines;

  size_t lines_count;

  struct function_addrs *function_addrs;
  size_t function_addrs_count;
};

struct unit_addrs {
  uint64_t low;
  uint64_t high;

  struct unit *u;
};

struct unit_addrs_vector {
  struct backtrace_vector vec;

  size_t count;
};

struct unit_vector {
  struct backtrace_vector vec;
  size_t count;
};

struct dwarf_data {
  struct dwarf_data *next;

  struct dwarf_data *altlink;

  uintptr_t base_address;

  struct unit_addrs *addrs;

  size_t addrs_count;

  struct unit **units;

  size_t units_count;

  struct dwarf_sections dwarf_sections;

  int is_bigendian;

  struct function_vector fvec;
};

static void dwarf_buf_error(struct dwarf_buf *buf, const char *msg,
                            int errnum) {
  char b[200];

  snprintf(b, sizeof b, "%s in %s at %d", msg, buf->name,
           (int)(buf->buf - buf->start));
  buf->error_callback(buf->data, b, errnum);
}

static int require(struct dwarf_buf *buf, size_t count) {
  if (buf->left >= count) return 1;

  if (!buf->reported_underflow) {
    dwarf_buf_error(buf, "DWARF underflow", 0);
    buf->reported_underflow = 1;
  }

  return 0;
}

static int advance(struct dwarf_buf *buf, size_t count) {
  if (!require(buf, count)) return 0;
  buf->buf += count;
  buf->left -= count;
  return 1;
}

static const char *read_string(struct dwarf_buf *buf) {
  const char *p = (const char *)buf->buf;
  size_t len = strnlen(p, buf->left);

  size_t count = len + 1;

  if (!advance(buf, count)) return NULL;

  return p;
}

static unsigned char read_byte(struct dwarf_buf *buf) {
  const unsigned char *p = buf->buf;

  if (!advance(buf, 1)) return 0;
  return p[0];
}

static signed char read_sbyte(struct dwarf_buf *buf) {
  const unsigned char *p = buf->buf;

  if (!advance(buf, 1)) return 0;
  return (*p ^ 0x80) - 0x80;
}

static uint16_t read_uint16(struct dwarf_buf *buf) {
  const unsigned char *p = buf->buf;

  if (!advance(buf, 2)) return 0;
  if (buf->is_bigendian)
    return ((uint16_t)p[0] << 8) | (uint16_t)p[1];
  else
    return ((uint16_t)p[1] << 8) | (uint16_t)p[0];
}

static uint32_t read_uint24(struct dwarf_buf *buf) {
  const unsigned char *p = buf->buf;

  if (!advance(buf, 3)) return 0;
  if (buf->is_bigendian)
    return (((uint32_t)p[0] << 16) | ((uint32_t)p[1] << 8) | (uint32_t)p[2]);
  else
    return (((uint32_t)p[2] << 16) | ((uint32_t)p[1] << 8) | (uint32_t)p[0]);
}

static uint32_t read_uint32(struct dwarf_buf *buf) {
  const unsigned char *p = buf->buf;

  if (!advance(buf, 4)) return 0;
  if (buf->is_bigendian)
    return (((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
            ((uint32_t)p[2] << 8) | (uint32_t)p[3]);
  else
    return (((uint32_t)p[3] << 24) | ((uint32_t)p[2] << 16) |
            ((uint32_t)p[1] << 8) | (uint32_t)p[0]);
}

static uint64_t read_uint64(struct dwarf_buf *buf) {
  const unsigned char *p = buf->buf;

  if (!advance(buf, 8)) return 0;
  if (buf->is_bigendian)
    return (((uint64_t)p[0] << 56) | ((uint64_t)p[1] << 48) |
            ((uint64_t)p[2] << 40) | ((uint64_t)p[3] << 32) |
            ((uint64_t)p[4] << 24) | ((uint64_t)p[5] << 16) |
            ((uint64_t)p[6] << 8) | (uint64_t)p[7]);
  else
    return (((uint64_t)p[7] << 56) | ((uint64_t)p[6] << 48) |
            ((uint64_t)p[5] << 40) | ((uint64_t)p[4] << 32) |
            ((uint64_t)p[3] << 24) | ((uint64_t)p[2] << 16) |
            ((uint64_t)p[1] << 8) | (uint64_t)p[0]);
}

static uint64_t read_offset(struct dwarf_buf *buf, int is_dwarf64) {
  if (is_dwarf64)
    return read_uint64(buf);
  else
    return read_uint32(buf);
}

static uint64_t read_address(struct dwarf_buf *buf, int addrsize) {
  switch (addrsize) {
    case 1:
      return read_byte(buf);
    case 2:
      return read_uint16(buf);
    case 4:
      return read_uint32(buf);
    case 8:
      return read_uint64(buf);
    default:
      dwarf_buf_error(buf, "unrecognized address size", 0);
      return 0;
  }
}

static int is_highest_address(uint64_t address, int addrsize) {
  switch (addrsize) {
    case 1:
      return address == (unsigned char)-1;
    case 2:
      return address == (uint16_t)-1;
    case 4:
      return address == (uint32_t)-1;
    case 8:
      return address == (uint64_t)-1;
    default:
      return 0;
  }
}

static uint64_t read_uleb128(struct dwarf_buf *buf) {
  uint64_t ret;
  unsigned int shift;
  int overflow;
  unsigned char b;

  ret = 0;
  shift = 0;
  overflow = 0;
  do {
    const unsigned char *p;

    p = buf->buf;
    if (!advance(buf, 1)) return 0;
    b = *p;
    if (shift < 64)
      ret |= ((uint64_t)(b & 0x7f)) << shift;
    else if (!overflow) {
      dwarf_buf_error(buf, "LEB128 overflows uint64_t", 0);
      overflow = 1;
    }
    shift += 7;
  } while ((b & 0x80) != 0);

  return ret;
}

static int64_t read_sleb128(struct dwarf_buf *buf) {
  uint64_t val;
  unsigned int shift;
  int overflow;
  unsigned char b;

  val = 0;
  shift = 0;
  overflow = 0;
  do {
    const unsigned char *p;

    p = buf->buf;
    if (!advance(buf, 1)) return 0;
    b = *p;
    if (shift < 64)
      val |= ((uint64_t)(b & 0x7f)) << shift;
    else if (!overflow) {
      dwarf_buf_error(buf, "signed LEB128 overflows uint64_t", 0);
      overflow = 1;
    }
    shift += 7;
  } while ((b & 0x80) != 0);

  if ((b & 0x40) != 0 && shift < 64) val |= ((uint64_t)-1) << shift;

  return (int64_t)val;
}

static size_t leb128_len(const unsigned char *p) {
  size_t ret;

  ret = 1;
  while ((*p & 0x80) != 0) {
    ++p;
    ++ret;
  }
  return ret;
}

static uint64_t read_initial_length(struct dwarf_buf *buf, int *is_dwarf64) {
  uint64_t len;

  len = read_uint32(buf);
  if (len == 0xffffffff) {
    len = read_uint64(buf);
    *is_dwarf64 = 1;
  } else
    *is_dwarf64 = 0;

  return len;
}

static void free_abbrevs(struct backtrace_state *state, struct abbrevs *abbrevs,
                         backtrace_error_callback error_callback, void *data) {
  size_t i;

  for (i = 0; i < abbrevs->num_abbrevs; ++i)
    backtrace_free(state, abbrevs->abbrevs[i].attrs,
                   abbrevs->abbrevs[i].num_attrs * sizeof(struct attr),
                   error_callback, data);
  backtrace_free(state, abbrevs->abbrevs,
                 abbrevs->num_abbrevs * sizeof(struct abbrev), error_callback,
                 data);
  abbrevs->num_abbrevs = 0;
  abbrevs->abbrevs = NULL;
}

static int read_attribute(enum dwarf_form form, uint64_t implicit_val,
                          struct dwarf_buf *buf, int is_dwarf64, int version,
                          int addrsize,
                          const struct dwarf_sections *dwarf_sections,
                          struct dwarf_data *altlink, struct attr_val *val) {
  memset(val, 0, sizeof *val);

  switch (form) {
    case DW_FORM_addr:
      val->encoding = ATTR_VAL_ADDRESS;
      val->u.uint = read_address(buf, addrsize);
      return 1;
    case DW_FORM_block2:
      val->encoding = ATTR_VAL_BLOCK;
      return advance(buf, read_uint16(buf));
    case DW_FORM_block4:
      val->encoding = ATTR_VAL_BLOCK;
      return advance(buf, read_uint32(buf));
    case DW_FORM_data2:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uint16(buf);
      return 1;
    case DW_FORM_data4:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uint32(buf);
      return 1;
    case DW_FORM_data8:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uint64(buf);
      return 1;
    case DW_FORM_data16:
      val->encoding = ATTR_VAL_BLOCK;
      return advance(buf, 16);
    case DW_FORM_string:
      val->encoding = ATTR_VAL_STRING;
      val->u.string = read_string(buf);
      return val->u.string == NULL ? 0 : 1;
    case DW_FORM_block:
      val->encoding = ATTR_VAL_BLOCK;
      return advance(buf, read_uleb128(buf));
    case DW_FORM_block1:
      val->encoding = ATTR_VAL_BLOCK;
      return advance(buf, read_byte(buf));
    case DW_FORM_data1:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_byte(buf);
      return 1;
    case DW_FORM_flag:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_byte(buf);
      return 1;
    case DW_FORM_sdata:
      val->encoding = ATTR_VAL_SINT;
      val->u.sint = read_sleb128(buf);
      return 1;
    case DW_FORM_strp: {
      uint64_t offset;

      offset = read_offset(buf, is_dwarf64);
      if (offset >= dwarf_sections->size[DEBUG_STR]) {
        dwarf_buf_error(buf, "DW_FORM_strp out of range", 0);
        return 0;
      }
      val->encoding = ATTR_VAL_STRING;
      val->u.string = (const char *)dwarf_sections->data[DEBUG_STR] + offset;
      return 1;
    }
    case DW_FORM_line_strp: {
      uint64_t offset;

      offset = read_offset(buf, is_dwarf64);
      if (offset >= dwarf_sections->size[DEBUG_LINE_STR]) {
        dwarf_buf_error(buf, "DW_FORM_line_strp out of range", 0);
        return 0;
      }
      val->encoding = ATTR_VAL_STRING;
      val->u.string =
          (const char *)dwarf_sections->data[DEBUG_LINE_STR] + offset;
      return 1;
    }
    case DW_FORM_udata:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = read_uleb128(buf);
      return 1;
    case DW_FORM_ref_addr:
      val->encoding = ATTR_VAL_REF_INFO;
      if (version == 2)
        val->u.uint = read_address(buf, addrsize);
      else
        val->u.uint = read_offset(buf, is_dwarf64);
      return 1;
    case DW_FORM_ref1:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_byte(buf);
      return 1;
    case DW_FORM_ref2:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uint16(buf);
      return 1;
    case DW_FORM_ref4:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uint32(buf);
      return 1;
    case DW_FORM_ref8:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uint64(buf);
      return 1;
    case DW_FORM_ref_udata:
      val->encoding = ATTR_VAL_REF_UNIT;
      val->u.uint = read_uleb128(buf);
      return 1;
    case DW_FORM_indirect: {
      uint64_t form;

      form = read_uleb128(buf);
      if (form == DW_FORM_implicit_const) {
        dwarf_buf_error(buf, "DW_FORM_indirect to DW_FORM_implicit_const", 0);
        return 0;
      }
      return read_attribute((enum dwarf_form)form, 0, buf, is_dwarf64, version,
                            addrsize, dwarf_sections, altlink, val);
    }
    case DW_FORM_sec_offset:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_offset(buf, is_dwarf64);
      return 1;
    case DW_FORM_exprloc:
      val->encoding = ATTR_VAL_EXPR;
      return advance(buf, read_uleb128(buf));
    case DW_FORM_flag_present:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = 1;
      return 1;
    case DW_FORM_ref_sig8:
      val->encoding = ATTR_VAL_REF_TYPE;
      val->u.uint = read_uint64(buf);
      return 1;
    case DW_FORM_strx:
    case DW_FORM_strx1:
    case DW_FORM_strx2:
    case DW_FORM_strx3:
    case DW_FORM_strx4: {
      uint64_t offset;

      switch (form) {
        case DW_FORM_strx:
          offset = read_uleb128(buf);
          break;
        case DW_FORM_strx1:
          offset = read_byte(buf);
          break;
        case DW_FORM_strx2:
          offset = read_uint16(buf);
          break;
        case DW_FORM_strx3:
          offset = read_uint24(buf);
          break;
        case DW_FORM_strx4:
          offset = read_uint32(buf);
          break;
        default:

          return 0;
      }
      val->encoding = ATTR_VAL_STRING_INDEX;
      val->u.uint = offset;
      return 1;
    }
    case DW_FORM_addrx:
    case DW_FORM_addrx1:
    case DW_FORM_addrx2:
    case DW_FORM_addrx3:
    case DW_FORM_addrx4: {
      uint64_t offset;

      switch (form) {
        case DW_FORM_addrx:
          offset = read_uleb128(buf);
          break;
        case DW_FORM_addrx1:
          offset = read_byte(buf);
          break;
        case DW_FORM_addrx2:
          offset = read_uint16(buf);
          break;
        case DW_FORM_addrx3:
          offset = read_uint24(buf);
          break;
        case DW_FORM_addrx4:
          offset = read_uint32(buf);
          break;
        default:

          return 0;
      }
      val->encoding = ATTR_VAL_ADDRESS_INDEX;
      val->u.uint = offset;
      return 1;
    }
    case DW_FORM_ref_sup4:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uint32(buf);
      return 1;
    case DW_FORM_ref_sup8:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uint64(buf);
      return 1;
    case DW_FORM_implicit_const:
      val->encoding = ATTR_VAL_UINT;
      val->u.uint = implicit_val;
      return 1;
    case DW_FORM_loclistx:

      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uleb128(buf);
      return 1;
    case DW_FORM_rnglistx:
      val->encoding = ATTR_VAL_RNGLISTS_INDEX;
      val->u.uint = read_uleb128(buf);
      return 1;
    case DW_FORM_GNU_addr_index:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uleb128(buf);
      return 1;
    case DW_FORM_GNU_str_index:
      val->encoding = ATTR_VAL_REF_SECTION;
      val->u.uint = read_uleb128(buf);
      return 1;
    case DW_FORM_GNU_ref_alt:
      val->u.uint = read_offset(buf, is_dwarf64);
      if (altlink == NULL) {
        val->encoding = ATTR_VAL_NONE;
        return 1;
      }
      val->encoding = ATTR_VAL_REF_ALT_INFO;
      return 1;
    case DW_FORM_strp_sup:
    case DW_FORM_GNU_strp_alt: {
      uint64_t offset;

      offset = read_offset(buf, is_dwarf64);
      if (altlink == NULL) {
        val->encoding = ATTR_VAL_NONE;
        return 1;
      }
      if (offset >= altlink->dwarf_sections.size[DEBUG_STR]) {
        dwarf_buf_error(buf, "DW_FORM_strp_sup out of range", 0);
        return 0;
      }
      val->encoding = ATTR_VAL_STRING;
      val->u.string =
          (const char *)altlink->dwarf_sections.data[DEBUG_STR] + offset;
      return 1;
    }
    default:
      dwarf_buf_error(buf, "unrecognized DWARF form", -1);
      return 0;
  }
}

static int resolve_string(const struct dwarf_sections *dwarf_sections,
                          int is_dwarf64, int is_bigendian,
                          uint64_t str_offsets_base, const struct attr_val *val,
                          backtrace_error_callback error_callback, void *data,
                          const char **string) {
  switch (val->encoding) {
    case ATTR_VAL_STRING:
      *string = val->u.string;
      return 1;

    case ATTR_VAL_STRING_INDEX: {
      uint64_t offset;
      struct dwarf_buf offset_buf;

      offset = val->u.uint * (is_dwarf64 ? 8 : 4) + str_offsets_base;
      if (offset + (is_dwarf64 ? 8 : 4) >
          dwarf_sections->size[DEBUG_STR_OFFSETS]) {
        error_callback(data, "DW_FORM_strx value out of range", 0);
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

      offset = read_offset(&offset_buf, is_dwarf64);
      if (offset >= dwarf_sections->size[DEBUG_STR]) {
        dwarf_buf_error(&offset_buf, "DW_FORM_strx offset out of range", 0);
        return 0;
      }
      *string = (const char *)dwarf_sections->data[DEBUG_STR] + offset;
      return 1;
    }

    default:
      return 1;
  }
}

static int resolve_addr_index(const struct dwarf_sections *dwarf_sections,
                              uint64_t addr_base, int addrsize,
                              int is_bigendian, uint64_t addr_index,
                              backtrace_error_callback error_callback,
                              void *data, uint64_t *address) {
  uint64_t offset;
  struct dwarf_buf addr_buf;

  offset = addr_index * addrsize + addr_base;
  if (offset + addrsize > dwarf_sections->size[DEBUG_ADDR]) {
    error_callback(data, "DW_FORM_addrx value out of range", 0);
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

  *address = read_address(&addr_buf, addrsize);
  return 1;
}

static int units_search(const void *vkey, const void *ventry) {
  const size_t *key = (const size_t *)vkey;
  const struct unit *entry = *((const struct unit *const *)ventry);
  size_t offset;

  offset = *key;
  if (offset < entry->low_offset)
    return -1;
  else if (offset >= entry->high_offset)
    return 1;
  else
    return 0;
}

static struct unit *find_unit(struct unit **pu, size_t units_count,
                              size_t offset) {
  struct unit **u;
  u = bsearch(&offset, pu, units_count, sizeof(struct unit *), units_search);
  return u == NULL ? NULL : *u;
}

static int function_addrs_compare(const void *v1, const void *v2) {
  const struct function_addrs *a1 = (const struct function_addrs *)v1;
  const struct function_addrs *a2 = (const struct function_addrs *)v2;

  if (a1->low < a2->low) return -1;
  if (a1->low > a2->low) return 1;
  if (a1->high < a2->high) return 1;
  if (a1->high > a2->high) return -1;
  return strcmp(a1->function->name, a2->function->name);
}

static int function_addrs_search(const void *vkey, const void *ventry) {
  const uintptr_t *key = (const uintptr_t *)vkey;
  const struct function_addrs *entry = (const struct function_addrs *)ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->low)
    return -1;
  else if (pc > (entry + 1)->low)
    return 1;
  else
    return 0;
}

static int add_unit_addr(struct backtrace_state *state, void *rdata,
                         uint64_t lowpc, uint64_t highpc,
                         backtrace_error_callback error_callback, void *data,
                         void *pvec) {
  struct unit *u = (struct unit *)rdata;
  struct unit_addrs_vector *vec = (struct unit_addrs_vector *)pvec;
  struct unit_addrs *p;

  if (vec->count > 0) {
    p = (struct unit_addrs *)vec->vec.base + (vec->count - 1);
    if ((lowpc == p->high || lowpc == p->high + 1) && u == p->u) {
      if (highpc > p->high) p->high = highpc;
      return 1;
    }
  }

  p = ((struct unit_addrs *)backtrace_vector_grow(
      state, sizeof(struct unit_addrs), error_callback, data, &vec->vec));
  if (p == NULL) return 0;

  p->low = lowpc;
  p->high = highpc;
  p->u = u;

  ++vec->count;

  return 1;
}

static int unit_addrs_compare(const void *v1, const void *v2) {
  const struct unit_addrs *a1 = (const struct unit_addrs *)v1;
  const struct unit_addrs *a2 = (const struct unit_addrs *)v2;

  if (a1->low < a2->low) return -1;
  if (a1->low > a2->low) return 1;
  if (a1->high < a2->high) return 1;
  if (a1->high > a2->high) return -1;
  if (a1->u->lineoff < a2->u->lineoff) return -1;
  if (a1->u->lineoff > a2->u->lineoff) return 1;
  return 0;
}

static int unit_addrs_search(const void *vkey, const void *ventry) {
  const uintptr_t *key = (const uintptr_t *)vkey;
  const struct unit_addrs *entry = (const struct unit_addrs *)ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->low)
    return -1;
  else if (pc > (entry + 1)->low)
    return 1;
  else
    return 0;
}

static int line_compare(const void *v1, const void *v2) {
  const struct line *ln1 = (const struct line *)v1;
  const struct line *ln2 = (const struct line *)v2;

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

static int line_search(const void *vkey, const void *ventry) {
  const uintptr_t *key = (const uintptr_t *)vkey;
  const struct line *entry = (const struct line *)ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->pc)
    return -1;
  else if (pc >= (entry + 1)->pc)
    return 1;
  else
    return 0;
}

static int abbrev_compare(const void *v1, const void *v2) {
  const struct abbrev *a1 = (const struct abbrev *)v1;
  const struct abbrev *a2 = (const struct abbrev *)v2;

  if (a1->code < a2->code)
    return -1;
  else if (a1->code > a2->code)
    return 1;
  else {
    return 0;
  }
}

static int read_abbrevs(struct backtrace_state *state, uint64_t abbrev_offset,
                        const unsigned char *dwarf_abbrev,
                        size_t dwarf_abbrev_size, int is_bigendian,
                        backtrace_error_callback error_callback, void *data,
                        struct abbrevs *abbrevs) {
  struct dwarf_buf abbrev_buf;
  struct dwarf_buf count_buf;
  size_t num_abbrevs;

  abbrevs->num_abbrevs = 0;
  abbrevs->abbrevs = NULL;

  if (abbrev_offset >= dwarf_abbrev_size) {
    error_callback(data, "abbrev offset out of range", 0);
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

  count_buf = abbrev_buf;
  num_abbrevs = 0;
  while (read_uleb128(&count_buf) != 0) {
    if (count_buf.reported_underflow) return 0;
    ++num_abbrevs;

    read_uleb128(&count_buf);

    read_byte(&count_buf);

    while (read_uleb128(&count_buf) != 0) {
      uint64_t form;

      form = read_uleb128(&count_buf);
      if ((enum dwarf_form)form == DW_FORM_implicit_const)
        read_sleb128(&count_buf);
    }

    read_uleb128(&count_buf);
  }

  if (count_buf.reported_underflow) return 0;

  if (num_abbrevs == 0) return 1;

  abbrevs->abbrevs = ((struct abbrev *)backtrace_alloc(
      state, num_abbrevs * sizeof(struct abbrev), error_callback, data));
  if (abbrevs->abbrevs == NULL) return 0;
  abbrevs->num_abbrevs = num_abbrevs;
  memset(abbrevs->abbrevs, 0, num_abbrevs * sizeof(struct abbrev));

  num_abbrevs = 0;
  while (1) {
    uint64_t code;
    struct abbrev a;
    size_t num_attrs;
    struct attr *attrs;

    if (abbrev_buf.reported_underflow) goto fail;

    code = read_uleb128(&abbrev_buf);
    if (code == 0) break;

    a.code = code;
    a.tag = (enum dwarf_tag)read_uleb128(&abbrev_buf);
    a.has_children = read_byte(&abbrev_buf);

    count_buf = abbrev_buf;
    num_attrs = 0;
    while (read_uleb128(&count_buf) != 0) {
      uint64_t form;

      ++num_attrs;
      form = read_uleb128(&count_buf);
      if ((enum dwarf_form)form == DW_FORM_implicit_const)
        read_sleb128(&count_buf);
    }

    if (num_attrs == 0) {
      attrs = NULL;
      read_uleb128(&abbrev_buf);
      read_uleb128(&abbrev_buf);
    } else {
      attrs = ((struct attr *)backtrace_alloc(state, num_attrs * sizeof *attrs,
                                              error_callback, data));
      if (attrs == NULL) goto fail;
      num_attrs = 0;
      while (1) {
        uint64_t name;
        uint64_t form;

        name = read_uleb128(&abbrev_buf);
        form = read_uleb128(&abbrev_buf);
        if (name == 0) break;
        attrs[num_attrs].name = (enum dwarf_attribute)name;
        attrs[num_attrs].form = (enum dwarf_form)form;
        if ((enum dwarf_form)form == DW_FORM_implicit_const)
          attrs[num_attrs].val = read_sleb128(&abbrev_buf);
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

  backtrace_qsort(abbrevs->abbrevs, abbrevs->num_abbrevs, sizeof(struct abbrev),
                  abbrev_compare);

  return 1;

fail:
  free_abbrevs(state, abbrevs, error_callback, data);
  return 0;
}

static const struct abbrev *lookup_abbrev(
    struct abbrevs *abbrevs, uint64_t code,
    backtrace_error_callback error_callback, void *data) {
  struct abbrev key;
  void *p;

  if (code - 1 < abbrevs->num_abbrevs &&
      abbrevs->abbrevs[code - 1].code == code)
    return &abbrevs->abbrevs[code - 1];

  memset(&key, 0, sizeof key);
  key.code = code;
  p = bsearch(&key, abbrevs->abbrevs, abbrevs->num_abbrevs,
              sizeof(struct abbrev), abbrev_compare);
  if (p == NULL) {
    error_callback(data, "invalid abbreviation code", 0);
    return NULL;
  }
  return (const struct abbrev *)p;
}

struct pcrange {
  uint64_t lowpc;
  int have_lowpc;
  int lowpc_is_addr_index;
  uint64_t highpc;
  int have_highpc;
  int highpc_is_relative;
  int highpc_is_addr_index;
  uint64_t ranges;
  int have_ranges;
  int ranges_is_index;
};

static void update_pcrange(const struct attr *attr, const struct attr_val *val,
                           struct pcrange *pcrange) {
  switch (attr->name) {
    case DW_AT_low_pc:
      if (val->encoding == ATTR_VAL_ADDRESS) {
        pcrange->lowpc = val->u.uint;
        pcrange->have_lowpc = 1;
      } else if (val->encoding == ATTR_VAL_ADDRESS_INDEX) {
        pcrange->lowpc = val->u.uint;
        pcrange->have_lowpc = 1;
        pcrange->lowpc_is_addr_index = 1;
      }
      break;

    case DW_AT_high_pc:
      if (val->encoding == ATTR_VAL_ADDRESS) {
        pcrange->highpc = val->u.uint;
        pcrange->have_highpc = 1;
      } else if (val->encoding == ATTR_VAL_UINT) {
        pcrange->highpc = val->u.uint;
        pcrange->have_highpc = 1;
        pcrange->highpc_is_relative = 1;
      } else if (val->encoding == ATTR_VAL_ADDRESS_INDEX) {
        pcrange->highpc = val->u.uint;
        pcrange->have_highpc = 1;
        pcrange->highpc_is_addr_index = 1;
      }
      break;

    case DW_AT_ranges:
      if (val->encoding == ATTR_VAL_UINT ||
          val->encoding == ATTR_VAL_REF_SECTION) {
        pcrange->ranges = val->u.uint;
        pcrange->have_ranges = 1;
      } else if (val->encoding == ATTR_VAL_RNGLISTS_INDEX) {
        pcrange->ranges = val->u.uint;
        pcrange->have_ranges = 1;
        pcrange->ranges_is_index = 1;
      }
      break;

    default:
      break;
  }
}

static int add_low_high_range(
    struct backtrace_state *state, const struct dwarf_sections *dwarf_sections,
    uintptr_t base_address, int is_bigendian, struct unit *u,
    const struct pcrange *pcrange,
    int (*add_range)(struct backtrace_state *state, void *rdata, uint64_t lowpc,
                     uint64_t highpc, backtrace_error_callback error_callback,
                     void *data, void *vec),
    void *rdata, backtrace_error_callback error_callback, void *data,
    void *vec) {
  uint64_t lowpc;
  uint64_t highpc;

  lowpc = pcrange->lowpc;
  if (pcrange->lowpc_is_addr_index) {
    if (!resolve_addr_index(dwarf_sections, u->addr_base, u->addrsize,
                            is_bigendian, lowpc, error_callback, data, &lowpc))
      return 0;
  }

  highpc = pcrange->highpc;
  if (pcrange->highpc_is_addr_index) {
    if (!resolve_addr_index(dwarf_sections, u->addr_base, u->addrsize,
                            is_bigendian, highpc, error_callback, data,
                            &highpc))
      return 0;
  }
  if (pcrange->highpc_is_relative) highpc += lowpc;

  lowpc += base_address;
  highpc += base_address;

  return add_range(state, rdata, lowpc, highpc, error_callback, data, vec);
}

static int add_ranges_from_ranges(
    struct backtrace_state *state, const struct dwarf_sections *dwarf_sections,
    uintptr_t base_address, int is_bigendian, struct unit *u, uint64_t base,
    const struct pcrange *pcrange,
    int (*add_range)(struct backtrace_state *state, void *rdata, uint64_t lowpc,
                     uint64_t highpc, backtrace_error_callback error_callback,
                     void *data, void *vec),
    void *rdata, backtrace_error_callback error_callback, void *data,
    void *vec) {
  struct dwarf_buf ranges_buf;

  if (pcrange->ranges >= dwarf_sections->size[DEBUG_RANGES]) {
    error_callback(data, "ranges offset out of range", 0);
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

  while (1) {
    uint64_t low;
    uint64_t high;

    if (ranges_buf.reported_underflow) return 0;

    low = read_address(&ranges_buf, u->addrsize);
    high = read_address(&ranges_buf, u->addrsize);

    if (low == 0 && high == 0) break;

    if (is_highest_address(low, u->addrsize))
      base = high;
    else {
      if (!add_range(state, rdata, low + base + base_address,
                     high + base + base_address, error_callback, data, vec))
        return 0;
    }
  }

  if (ranges_buf.reported_underflow) return 0;

  return 1;
}

static int add_ranges_from_rnglists(
    struct backtrace_state *state, const struct dwarf_sections *dwarf_sections,
    uintptr_t base_address, int is_bigendian, struct unit *u, uint64_t base,
    const struct pcrange *pcrange,
    int (*add_range)(struct backtrace_state *state, void *rdata, uint64_t lowpc,
                     uint64_t highpc, backtrace_error_callback error_callback,
                     void *data, void *vec),
    void *rdata, backtrace_error_callback error_callback, void *data,
    void *vec) {
  uint64_t offset;
  struct dwarf_buf rnglists_buf;

  if (!pcrange->ranges_is_index)
    offset = pcrange->ranges;
  else
    offset = u->rnglists_base + pcrange->ranges * (u->is_dwarf64 ? 8 : 4);
  if (offset >= dwarf_sections->size[DEBUG_RNGLISTS]) {
    error_callback(data, "rnglists offset out of range", 0);
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

  if (pcrange->ranges_is_index) {
    offset = read_offset(&rnglists_buf, u->is_dwarf64);
    offset += u->rnglists_base;
    if (offset >= dwarf_sections->size[DEBUG_RNGLISTS]) {
      error_callback(data, "rnglists index offset out of range", 0);
      return 0;
    }
    rnglists_buf.buf = dwarf_sections->data[DEBUG_RNGLISTS] + offset;
    rnglists_buf.left = dwarf_sections->size[DEBUG_RNGLISTS] - offset;
  }

  while (1) {
    unsigned char rle;

    rle = read_byte(&rnglists_buf);
    if (rle == DW_RLE_end_of_list) break;
    switch (rle) {
      case DW_RLE_base_addressx: {
        uint64_t index;

        index = read_uleb128(&rnglists_buf);
        if (!resolve_addr_index(dwarf_sections, u->addr_base, u->addrsize,
                                is_bigendian, index, error_callback, data,
                                &base))
          return 0;
      } break;

      case DW_RLE_startx_endx: {
        uint64_t index;
        uint64_t low;
        uint64_t high;

        index = read_uleb128(&rnglists_buf);
        if (!resolve_addr_index(dwarf_sections, u->addr_base, u->addrsize,
                                is_bigendian, index, error_callback, data,
                                &low))
          return 0;
        index = read_uleb128(&rnglists_buf);
        if (!resolve_addr_index(dwarf_sections, u->addr_base, u->addrsize,
                                is_bigendian, index, error_callback, data,
                                &high))
          return 0;
        if (!add_range(state, rdata, low + base_address, high + base_address,
                       error_callback, data, vec))
          return 0;
      } break;

      case DW_RLE_startx_length: {
        uint64_t index;
        uint64_t low;
        uint64_t length;

        index = read_uleb128(&rnglists_buf);
        if (!resolve_addr_index(dwarf_sections, u->addr_base, u->addrsize,
                                is_bigendian, index, error_callback, data,
                                &low))
          return 0;
        length = read_uleb128(&rnglists_buf);
        low += base_address;
        if (!add_range(state, rdata, low, low + length, error_callback, data,
                       vec))
          return 0;
      } break;

      case DW_RLE_offset_pair: {
        uint64_t low;
        uint64_t high;

        low = read_uleb128(&rnglists_buf);
        high = read_uleb128(&rnglists_buf);
        if (!add_range(state, rdata, low + base + base_address,
                       high + base + base_address, error_callback, data, vec))
          return 0;
      } break;

      case DW_RLE_base_address:
        base = read_address(&rnglists_buf, u->addrsize);
        break;

      case DW_RLE_start_end: {
        uint64_t low;
        uint64_t high;

        low = read_address(&rnglists_buf, u->addrsize);
        high = read_address(&rnglists_buf, u->addrsize);
        if (!add_range(state, rdata, low + base_address, high + base_address,
                       error_callback, data, vec))
          return 0;
      } break;

      case DW_RLE_start_length: {
        uint64_t low;
        uint64_t length;

        low = read_address(&rnglists_buf, u->addrsize);
        length = read_uleb128(&rnglists_buf);
        low += base_address;
        if (!add_range(state, rdata, low, low + length, error_callback, data,
                       vec))
          return 0;
      } break;

      default:
        dwarf_buf_error(&rnglists_buf, "unrecognized DW_RLE value", -1);
        return 0;
    }
  }

  if (rnglists_buf.reported_underflow) return 0;

  return 1;
}

static int add_ranges(
    struct backtrace_state *state, const struct dwarf_sections *dwarf_sections,
    uintptr_t base_address, int is_bigendian, struct unit *u, uint64_t base,
    const struct pcrange *pcrange,
    int (*add_range)(struct backtrace_state *state, void *rdata, uint64_t lowpc,
                     uint64_t highpc, backtrace_error_callback error_callback,
                     void *data, void *vec),
    void *rdata, backtrace_error_callback error_callback, void *data,
    void *vec) {
  if (pcrange->have_lowpc && pcrange->have_highpc)
    return add_low_high_range(state, dwarf_sections, base_address, is_bigendian,
                              u, pcrange, add_range, rdata, error_callback,
                              data, vec);

  if (!pcrange->have_ranges) {
    return 1;
  }

  if (u->version < 5)
    return add_ranges_from_ranges(state, dwarf_sections, base_address,
                                  is_bigendian, u, base, pcrange, add_range,
                                  rdata, error_callback, data, vec);
  else
    return add_ranges_from_rnglists(state, dwarf_sections, base_address,
                                    is_bigendian, u, base, pcrange, add_range,
                                    rdata, error_callback, data, vec);
}

static int find_address_ranges(
    struct backtrace_state *state, uintptr_t base_address,
    struct dwarf_buf *unit_buf, const struct dwarf_sections *dwarf_sections,
    int is_bigendian, struct dwarf_data *altlink,
    backtrace_error_callback error_callback, void *data, struct unit *u,
    struct unit_addrs_vector *addrs, enum dwarf_tag *unit_tag) {
  while (unit_buf->left > 0) {
    uint64_t code;
    const struct abbrev *abbrev;
    struct pcrange pcrange;
    struct attr_val name_val;
    int have_name_val;
    struct attr_val comp_dir_val;
    int have_comp_dir_val;
    size_t i;

    code = read_uleb128(unit_buf);
    if (code == 0) return 1;

    abbrev = lookup_abbrev(&u->abbrevs, code, error_callback, data);
    if (abbrev == NULL) return 0;

    if (unit_tag != NULL) *unit_tag = abbrev->tag;

    memset(&pcrange, 0, sizeof pcrange);
    memset(&name_val, 0, sizeof name_val);
    have_name_val = 0;
    memset(&comp_dir_val, 0, sizeof comp_dir_val);
    have_comp_dir_val = 0;
    for (i = 0; i < abbrev->num_attrs; ++i) {
      struct attr_val val;

      if (!read_attribute(abbrev->attrs[i].form, abbrev->attrs[i].val, unit_buf,
                          u->is_dwarf64, u->version, u->addrsize,
                          dwarf_sections, altlink, &val))
        return 0;

      switch (abbrev->attrs[i].name) {
        case DW_AT_low_pc:
        case DW_AT_high_pc:
        case DW_AT_ranges:
          update_pcrange(&abbrev->attrs[i], &val, &pcrange);
          break;

        case DW_AT_stmt_list:
          if ((abbrev->tag == DW_TAG_compile_unit ||
               abbrev->tag == DW_TAG_skeleton_unit) &&
              (val.encoding == ATTR_VAL_UINT ||
               val.encoding == ATTR_VAL_REF_SECTION))
            u->lineoff = val.u.uint;
          break;

        case DW_AT_name:
          if (abbrev->tag == DW_TAG_compile_unit ||
              abbrev->tag == DW_TAG_skeleton_unit) {
            name_val = val;
            have_name_val = 1;
          }
          break;

        case DW_AT_comp_dir:
          if (abbrev->tag == DW_TAG_compile_unit ||
              abbrev->tag == DW_TAG_skeleton_unit) {
            comp_dir_val = val;
            have_comp_dir_val = 1;
          }
          break;

        case DW_AT_str_offsets_base:
          if ((abbrev->tag == DW_TAG_compile_unit ||
               abbrev->tag == DW_TAG_skeleton_unit) &&
              val.encoding == ATTR_VAL_REF_SECTION)
            u->str_offsets_base = val.u.uint;
          break;

        case DW_AT_addr_base:
          if ((abbrev->tag == DW_TAG_compile_unit ||
               abbrev->tag == DW_TAG_skeleton_unit) &&
              val.encoding == ATTR_VAL_REF_SECTION)
            u->addr_base = val.u.uint;
          break;

        case DW_AT_rnglists_base:
          if ((abbrev->tag == DW_TAG_compile_unit ||
               abbrev->tag == DW_TAG_skeleton_unit) &&
              val.encoding == ATTR_VAL_REF_SECTION)
            u->rnglists_base = val.u.uint;
          break;

        default:
          break;
      }
    }

    if (have_name_val) {
      if (!resolve_string(dwarf_sections, u->is_dwarf64, is_bigendian,
                          u->str_offsets_base, &name_val, error_callback, data,
                          &u->filename))
        return 0;
    }
    if (have_comp_dir_val) {
      if (!resolve_string(dwarf_sections, u->is_dwarf64, is_bigendian,
                          u->str_offsets_base, &comp_dir_val, error_callback,
                          data, &u->comp_dir))
        return 0;
    }

    if (abbrev->tag == DW_TAG_compile_unit ||
        abbrev->tag == DW_TAG_subprogram ||
        abbrev->tag == DW_TAG_skeleton_unit) {
      if (!add_ranges(state, dwarf_sections, base_address, is_bigendian, u,
                      pcrange.lowpc, &pcrange, add_unit_addr, (void *)u,
                      error_callback, data, (void *)addrs))
        return 0;

      if ((abbrev->tag == DW_TAG_compile_unit ||
           abbrev->tag == DW_TAG_skeleton_unit) &&
          (pcrange.have_ranges || (pcrange.have_lowpc && pcrange.have_highpc)))
        return 1;
    }

    if (abbrev->has_children) {
      if (!find_address_ranges(state, base_address, unit_buf, dwarf_sections,
                               is_bigendian, altlink, error_callback, data, u,
                               addrs, NULL))
        return 0;
    }
  }

  return 1;
}

static int build_address_map(struct backtrace_state *state,
                             uintptr_t base_address,
                             const struct dwarf_sections *dwarf_sections,
                             int is_bigendian, struct dwarf_data *altlink,
                             backtrace_error_callback error_callback,
                             void *data, struct unit_addrs_vector *addrs,
                             struct unit_vector *unit_vec) {
  struct dwarf_buf info;
  struct backtrace_vector units;
  size_t units_count;
  size_t i;
  struct unit **pu;
  size_t unit_offset = 0;
  struct unit_addrs *pa;

  memset(&addrs->vec, 0, sizeof addrs->vec);
  memset(&unit_vec->vec, 0, sizeof unit_vec->vec);
  addrs->count = 0;
  unit_vec->count = 0;

  info.name = ".debug_info";
  info.start = dwarf_sections->data[DEBUG_INFO];
  info.buf = info.start;
  info.left = dwarf_sections->size[DEBUG_INFO];
  info.is_bigendian = is_bigendian;
  info.error_callback = error_callback;
  info.data = data;
  info.reported_underflow = 0;

  memset(&units, 0, sizeof units);
  units_count = 0;

  while (info.left > 0) {
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

    if (info.reported_underflow) goto fail;

    unit_data_start = info.buf;

    len = read_initial_length(&info, &is_dwarf64);
    unit_buf = info;
    unit_buf.left = len;

    if (!advance(&info, len)) goto fail;

    version = read_uint16(&unit_buf);
    if (version < 2 || version > 5) {
      dwarf_buf_error(&unit_buf, "unrecognized DWARF version", -1);
      goto fail;
    }

    if (version < 5)
      unit_type = 0;
    else {
      unit_type = read_byte(&unit_buf);
      if (unit_type == DW_UT_type || unit_type == DW_UT_split_type) {
        continue;
      }
    }

    pu = ((struct unit **)backtrace_vector_grow(state, sizeof(struct unit *),
                                                error_callback, data, &units));
    if (pu == NULL) goto fail;

    u = ((struct unit *)backtrace_alloc(state, sizeof *u, error_callback,
                                        data));
    if (u == NULL) goto fail;

    *pu = u;
    ++units_count;

    if (version < 5)
      addrsize = 0;
    else
      addrsize = read_byte(&unit_buf);

    memset(&u->abbrevs, 0, sizeof u->abbrevs);
    abbrev_offset = read_offset(&unit_buf, is_dwarf64);
    if (!read_abbrevs(state, abbrev_offset, dwarf_sections->data[DEBUG_ABBREV],
                      dwarf_sections->size[DEBUG_ABBREV], is_bigendian,
                      error_callback, data, &u->abbrevs))
      goto fail;

    if (version < 5) addrsize = read_byte(&unit_buf);

    switch (unit_type) {
      case 0:
        break;
      case DW_UT_compile:
      case DW_UT_partial:
        break;
      case DW_UT_skeleton:
      case DW_UT_split_compile:
        read_uint64(&unit_buf);
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

    u->lines = NULL;
    u->lines_count = 0;
    u->function_addrs = NULL;
    u->function_addrs_count = 0;

    if (!find_address_ranges(state, base_address, &unit_buf, dwarf_sections,
                             is_bigendian, altlink, error_callback, data, u,
                             addrs, &unit_tag))
      goto fail;

    if (unit_buf.reported_underflow) goto fail;
  }
  if (info.reported_underflow) goto fail;

  pa = ((struct unit_addrs *)backtrace_vector_grow(
      state, sizeof(struct unit_addrs), error_callback, data, &addrs->vec));
  if (pa == NULL) goto fail;
  pa->low = 0;
  --pa->low;
  pa->high = pa->low;
  pa->u = NULL;

  unit_vec->vec = units;
  unit_vec->count = units_count;
  return 1;

fail:
  if (units_count > 0) {
    pu = (struct unit **)units.base;
    for (i = 0; i < units_count; i++) {
      free_abbrevs(state, &pu[i]->abbrevs, error_callback, data);
      backtrace_free(state, pu[i], sizeof **pu, error_callback, data);
    }
    backtrace_vector_free(state, &units, error_callback, data);
  }
  if (addrs->count > 0) {
    backtrace_vector_free(state, &addrs->vec, error_callback, data);
    addrs->count = 0;
  }
  return 0;
}

static int add_line(struct backtrace_state *state, struct dwarf_data *ddata,
                    uintptr_t pc, const char *filename, int lineno,
                    backtrace_error_callback error_callback, void *data,
                    struct line_vector *vec) {
  struct line *ln;

  if (vec->count > 0) {
    ln = (struct line *)vec->vec.base + (vec->count - 1);
    if (pc == ln->pc && filename == ln->filename && lineno == ln->lineno)
      return 1;
  }

  ln = ((struct line *)backtrace_vector_grow(state, sizeof(struct line),
                                             error_callback, data, &vec->vec));
  if (ln == NULL) return 0;

  ln->pc = pc + ddata->base_address;

  ln->filename = filename;
  ln->lineno = lineno;
  ln->idx = vec->count;

  ++vec->count;

  return 1;
}

static void free_line_header(struct backtrace_state *state,
                             struct line_header *hdr,
                             backtrace_error_callback error_callback,
                             void *data) {
  if (hdr->dirs_count != 0)
    backtrace_free(state, hdr->dirs, hdr->dirs_count * sizeof(const char *),
                   error_callback, data);
  backtrace_free(state, hdr->filenames, hdr->filenames_count * sizeof(char *),
                 error_callback, data);
}

static int read_v2_paths(struct backtrace_state *state, struct unit *u,
                         struct dwarf_buf *hdr_buf, struct line_header *hdr) {
  const unsigned char *p;
  const unsigned char *pend;
  size_t i;

  hdr->dirs_count = 0;
  p = hdr_buf->buf;
  pend = p + hdr_buf->left;
  while (p < pend && *p != '\0') {
    p += strnlen((const char *)p, pend - p) + 1;
    ++hdr->dirs_count;
  }

  ++hdr->dirs_count;
  hdr->dirs = ((const char **)backtrace_alloc(
      state, hdr->dirs_count * sizeof(const char *), hdr_buf->error_callback,
      hdr_buf->data));
  if (hdr->dirs == NULL) return 0;

  hdr->dirs[0] = u->comp_dir;
  i = 1;
  while (*hdr_buf->buf != '\0') {
    if (hdr_buf->reported_underflow) return 0;

    hdr->dirs[i] = read_string(hdr_buf);
    if (hdr->dirs[i] == NULL) return 0;
    ++i;
  }
  if (!advance(hdr_buf, 1)) return 0;

  hdr->filenames_count = 0;
  p = hdr_buf->buf;
  pend = p + hdr_buf->left;
  while (p < pend && *p != '\0') {
    p += strnlen((const char *)p, pend - p) + 1;
    p += leb128_len(p);
    p += leb128_len(p);
    p += leb128_len(p);
    ++hdr->filenames_count;
  }

  ++hdr->filenames_count;
  hdr->filenames = ((const char **)backtrace_alloc(
      state, hdr->filenames_count * sizeof(char *), hdr_buf->error_callback,
      hdr_buf->data));
  if (hdr->filenames == NULL) return 0;
  hdr->filenames[0] = u->filename;
  i = 1;
  while (*hdr_buf->buf != '\0') {
    const char *filename;
    uint64_t dir_index;

    if (hdr_buf->reported_underflow) return 0;

    filename = read_string(hdr_buf);
    if (filename == NULL) return 0;
    dir_index = read_uleb128(hdr_buf);
    if (IS_ABSOLUTE_PATH(filename) ||
        (dir_index < hdr->dirs_count && hdr->dirs[dir_index] == NULL))
      hdr->filenames[i] = filename;
    else {
      const char *dir;
      size_t dir_len;
      size_t filename_len;
      char *s;

      if (dir_index < hdr->dirs_count)
        dir = hdr->dirs[dir_index];
      else {
        dwarf_buf_error(hdr_buf,
                        ("invalid directory index in "
                         "line number program header"),
                        0);
        return 0;
      }
      dir_len = strlen(dir);
      filename_len = strlen(filename);
      s = ((char *)backtrace_alloc(state, dir_len + filename_len + 2,
                                   hdr_buf->error_callback, hdr_buf->data));
      if (s == NULL) return 0;
      memcpy(s, dir, dir_len);

      s[dir_len] = '/';
      memcpy(s + dir_len + 1, filename, filename_len + 1);
      hdr->filenames[i] = s;
    }

    read_uleb128(hdr_buf);
    read_uleb128(hdr_buf);

    ++i;
  }

  return 1;
}

static int read_lnct(struct backtrace_state *state, struct dwarf_data *ddata,
                     struct unit *u, struct dwarf_buf *hdr_buf,
                     const struct line_header *hdr, size_t formats_count,
                     const struct line_header_format *formats,
                     const char **string) {
  size_t i;
  const char *dir;
  const char *path;

  dir = NULL;
  path = NULL;
  for (i = 0; i < formats_count; i++) {
    struct attr_val val;

    if (!read_attribute(formats[i].form, 0, hdr_buf, u->is_dwarf64, u->version,
                        hdr->addrsize, &ddata->dwarf_sections, ddata->altlink,
                        &val))
      return 0;
    switch (formats[i].lnct) {
      case DW_LNCT_path:
        if (!resolve_string(&ddata->dwarf_sections, u->is_dwarf64,
                            ddata->is_bigendian, u->str_offsets_base, &val,
                            hdr_buf->error_callback, hdr_buf->data, &path))
          return 0;
        break;
      case DW_LNCT_directory_index:
        if (val.encoding == ATTR_VAL_UINT) {
          if (val.u.uint >= hdr->dirs_count) {
            dwarf_buf_error(hdr_buf,
                            ("invalid directory index in "
                             "line number program header"),
                            0);
            return 0;
          }
          dir = hdr->dirs[val.u.uint];
        }
        break;
      default:

        break;
    }
  }

  if (path == NULL) {
    dwarf_buf_error(hdr_buf, "missing file name in line number program header",
                    0);
    return 0;
  }

  if (dir == NULL)
    *string = path;
  else {
    size_t dir_len;
    size_t path_len;
    char *s;

    dir_len = strlen(dir);
    path_len = strlen(path);
    s = (char *)backtrace_alloc(state, dir_len + path_len + 2,
                                hdr_buf->error_callback, hdr_buf->data);
    if (s == NULL) return 0;
    memcpy(s, dir, dir_len);

    s[dir_len] = '/';
    memcpy(s + dir_len + 1, path, path_len + 1);
    *string = s;
  }

  return 1;
}

static int read_line_header_format_entries(
    struct backtrace_state *state, struct dwarf_data *ddata, struct unit *u,
    struct dwarf_buf *hdr_buf, struct line_header *hdr, size_t *pcount,
    const char ***ppaths) {
  size_t formats_count;
  struct line_header_format *formats;
  size_t paths_count;
  const char **paths;
  size_t i;
  int ret;

  formats_count = read_byte(hdr_buf);
  if (formats_count == 0)
    formats = NULL;
  else {
    formats = ((struct line_header_format *)backtrace_alloc(
        state, (formats_count * sizeof(struct line_header_format)),
        hdr_buf->error_callback, hdr_buf->data));
    if (formats == NULL) return 0;

    for (i = 0; i < formats_count; i++) {
      formats[i].lnct = (int)read_uleb128(hdr_buf);
      formats[i].form = (enum dwarf_form)read_uleb128(hdr_buf);
    }
  }

  paths_count = read_uleb128(hdr_buf);
  if (paths_count == 0) {
    *pcount = 0;
    *ppaths = NULL;
    ret = 1;
    goto exit;
  }

  paths =
      ((const char **)backtrace_alloc(state, paths_count * sizeof(const char *),
                                      hdr_buf->error_callback, hdr_buf->data));
  if (paths == NULL) {
    ret = 0;
    goto exit;
  }
  for (i = 0; i < paths_count; i++) {
    if (!read_lnct(state, ddata, u, hdr_buf, hdr, formats_count, formats,
                   &paths[i])) {
      backtrace_free(state, paths, paths_count * sizeof(const char *),
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
    backtrace_free(state, formats,
                   formats_count * sizeof(struct line_header_format),
                   hdr_buf->error_callback, hdr_buf->data);

  return ret;
}

static int read_line_header(struct backtrace_state *state,
                            struct dwarf_data *ddata, struct unit *u,
                            int is_dwarf64, struct dwarf_buf *line_buf,
                            struct line_header *hdr) {
  uint64_t hdrlen;
  struct dwarf_buf hdr_buf;

  hdr->version = read_uint16(line_buf);
  if (hdr->version < 2 || hdr->version > 5) {
    dwarf_buf_error(line_buf, "unsupported line number version", -1);
    return 0;
  }

  if (hdr->version < 5)
    hdr->addrsize = u->addrsize;
  else {
    hdr->addrsize = read_byte(line_buf);

    if (read_byte(line_buf) != 0) {
      dwarf_buf_error(line_buf, "non-zero segment_selector_size not supported",
                      -1);
      return 0;
    }
  }

  hdrlen = read_offset(line_buf, is_dwarf64);

  hdr_buf = *line_buf;
  hdr_buf.left = hdrlen;

  if (!advance(line_buf, hdrlen)) return 0;

  hdr->min_insn_len = read_byte(&hdr_buf);
  if (hdr->version < 4)
    hdr->max_ops_per_insn = 1;
  else
    hdr->max_ops_per_insn = read_byte(&hdr_buf);

  read_byte(&hdr_buf);

  hdr->line_base = read_sbyte(&hdr_buf);
  hdr->line_range = read_byte(&hdr_buf);

  hdr->opcode_base = read_byte(&hdr_buf);
  hdr->opcode_lengths = hdr_buf.buf;
  if (!advance(&hdr_buf, hdr->opcode_base - 1)) return 0;

  if (hdr->version < 5) {
    if (!read_v2_paths(state, u, &hdr_buf, hdr)) return 0;
  } else {
    if (!read_line_header_format_entries(state, ddata, u, &hdr_buf, hdr,
                                         &hdr->dirs_count, &hdr->dirs))
      return 0;
    if (!read_line_header_format_entries(state, ddata, u, &hdr_buf, hdr,
                                         &hdr->filenames_count,
                                         &hdr->filenames))
      return 0;
  }

  if (hdr_buf.reported_underflow) return 0;

  return 1;
}

static int read_line_program(struct backtrace_state *state,
                             struct dwarf_data *ddata,
                             const struct line_header *hdr,
                             struct dwarf_buf *line_buf,
                             struct line_vector *vec) {
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
  while (line_buf->left > 0) {
    unsigned int op;

    op = read_byte(line_buf);
    if (op >= hdr->opcode_base) {
      unsigned int advance;

      op -= hdr->opcode_base;
      advance = op / hdr->line_range;
      address +=
          (hdr->min_insn_len * (op_index + advance) / hdr->max_ops_per_insn);
      op_index = (op_index + advance) % hdr->max_ops_per_insn;
      lineno += hdr->line_base + (int)(op % hdr->line_range);
      add_line(state, ddata, address, filename, lineno,
               line_buf->error_callback, line_buf->data, vec);
    } else if (op == DW_LNS_extended_op) {
      uint64_t len;

      len = read_uleb128(line_buf);
      op = read_byte(line_buf);
      switch (op) {
        case DW_LNE_end_sequence:

          address = 0;
          op_index = 0;
          filename = reset_filename;
          lineno = 1;
          break;
        case DW_LNE_set_address:
          address = read_address(line_buf, hdr->addrsize);
          break;
        case DW_LNE_define_file: {
          const char *f;
          unsigned int dir_index;

          f = read_string(line_buf);
          if (f == NULL) return 0;
          dir_index = read_uleb128(line_buf);

          read_uleb128(line_buf);
          read_uleb128(line_buf);
          if (IS_ABSOLUTE_PATH(f))
            filename = f;
          else {
            const char *dir;
            size_t dir_len;
            size_t f_len;
            char *p;

            if (dir_index < hdr->dirs_count)
              dir = hdr->dirs[dir_index];
            else {
              dwarf_buf_error(line_buf,
                              ("invalid directory index "
                               "in line number program"),
                              0);
              return 0;
            }
            dir_len = strlen(dir);
            f_len = strlen(f);
            p = ((char *)backtrace_alloc(state, dir_len + f_len + 2,
                                         line_buf->error_callback,
                                         line_buf->data));
            if (p == NULL) return 0;
            memcpy(p, dir, dir_len);

            p[dir_len] = '/';
            memcpy(p + dir_len + 1, f, f_len + 1);
            filename = p;
          }
        } break;
        case DW_LNE_set_discriminator:

          read_uleb128(line_buf);
          break;
        default:
          if (!advance(line_buf, len - 1)) return 0;
          break;
      }
    } else {
      switch (op) {
        case DW_LNS_copy:
          add_line(state, ddata, address, filename, lineno,
                   line_buf->error_callback, line_buf->data, vec);
          break;
        case DW_LNS_advance_pc: {
          uint64_t advance;

          advance = read_uleb128(line_buf);
          address += (hdr->min_insn_len * (op_index + advance) /
                      hdr->max_ops_per_insn);
          op_index = (op_index + advance) % hdr->max_ops_per_insn;
        } break;
        case DW_LNS_advance_line:
          lineno += (int)read_sleb128(line_buf);
          break;
        case DW_LNS_set_file: {
          uint64_t fileno;

          fileno = read_uleb128(line_buf);
          if (fileno >= hdr->filenames_count) {
            dwarf_buf_error(line_buf,
                            ("invalid file number in "
                             "line number program"),
                            0);
            return 0;
          }
          filename = hdr->filenames[fileno];
        } break;
        case DW_LNS_set_column:
          read_uleb128(line_buf);
          break;
        case DW_LNS_negate_stmt:
          break;
        case DW_LNS_set_basic_block:
          break;
        case DW_LNS_const_add_pc: {
          unsigned int advance;

          op = 255 - hdr->opcode_base;
          advance = op / hdr->line_range;
          address += (hdr->min_insn_len * (op_index + advance) /
                      hdr->max_ops_per_insn);
          op_index = (op_index + advance) % hdr->max_ops_per_insn;
        } break;
        case DW_LNS_fixed_advance_pc:
          address += read_uint16(line_buf);
          op_index = 0;
          break;
        case DW_LNS_set_prologue_end:
          break;
        case DW_LNS_set_epilogue_begin:
          break;
        case DW_LNS_set_isa:
          read_uleb128(line_buf);
          break;
        default: {
          unsigned int i;

          for (i = hdr->opcode_lengths[op - 1]; i > 0; --i)
            read_uleb128(line_buf);
        } break;
      }
    }
  }

  return 1;
}

static int read_line_info(struct backtrace_state *state,
                          struct dwarf_data *ddata,
                          backtrace_error_callback error_callback, void *data,
                          struct unit *u, struct line_header *hdr,
                          struct line **lines, size_t *lines_count) {
  struct line_vector vec;
  struct dwarf_buf line_buf;
  uint64_t len;
  int is_dwarf64;
  struct line *ln;

  memset(&vec.vec, 0, sizeof vec.vec);
  vec.count = 0;

  memset(hdr, 0, sizeof *hdr);

  if (u->lineoff != (off_t)(size_t)u->lineoff ||
      (size_t)u->lineoff >= ddata->dwarf_sections.size[DEBUG_LINE]) {
    error_callback(data, "unit line offset out of range", 0);
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

  len = read_initial_length(&line_buf, &is_dwarf64);
  line_buf.left = len;

  if (!read_line_header(state, ddata, u, is_dwarf64, &line_buf, hdr)) goto fail;

  if (!read_line_program(state, ddata, hdr, &line_buf, &vec)) goto fail;

  if (line_buf.reported_underflow) goto fail;

  if (vec.count == 0) {
    goto fail;
  }

  ln = ((struct line *)backtrace_vector_grow(state, sizeof(struct line),
                                             error_callback, data, &vec.vec));
  if (ln == NULL) goto fail;
  ln->pc = (uintptr_t)-1;
  ln->filename = NULL;
  ln->lineno = 0;
  ln->idx = 0;

  if (!backtrace_vector_release(state, &vec.vec, error_callback, data))
    goto fail;

  ln = (struct line *)vec.vec.base;
  backtrace_qsort(ln, vec.count, sizeof(struct line), line_compare);

  *lines = ln;
  *lines_count = vec.count;

  return 1;

fail:
  backtrace_vector_free(state, &vec.vec, error_callback, data);
  free_line_header(state, hdr, error_callback, data);
  *lines = (struct line *)(uintptr_t)-1;
  *lines_count = 0;
  return 0;
}

static const char *read_referenced_name(struct dwarf_data *, struct unit *,
                                        uint64_t, backtrace_error_callback,
                                        void *);

static const char *read_referenced_name_from_attr(
    struct dwarf_data *ddata, struct unit *u, struct attr *attr,
    struct attr_val *val, backtrace_error_callback error_callback, void *data) {
  switch (attr->name) {
    case DW_AT_abstract_origin:
    case DW_AT_specification:
      break;
    default:
      return NULL;
  }

  if (attr->form == DW_FORM_ref_sig8) return NULL;

  if (val->encoding == ATTR_VAL_REF_INFO) {
    struct unit *unit =
        find_unit(ddata->units, ddata->units_count, val->u.uint);
    if (unit == NULL) return NULL;

    uint64_t offset = val->u.uint - unit->low_offset;
    return read_referenced_name(ddata, unit, offset, error_callback, data);
  }

  if (val->encoding == ATTR_VAL_UINT || val->encoding == ATTR_VAL_REF_UNIT)
    return read_referenced_name(ddata, u, val->u.uint, error_callback, data);

  if (val->encoding == ATTR_VAL_REF_ALT_INFO) {
    struct unit *alt_unit = find_unit(ddata->altlink->units,
                                      ddata->altlink->units_count, val->u.uint);
    if (alt_unit == NULL) return NULL;

    uint64_t offset = val->u.uint - alt_unit->low_offset;
    return read_referenced_name(ddata->altlink, alt_unit, offset,
                                error_callback, data);
  }

  return NULL;
}

static const char *read_referenced_name(struct dwarf_data *ddata,
                                        struct unit *u, uint64_t offset,
                                        backtrace_error_callback error_callback,
                                        void *data) {
  struct dwarf_buf unit_buf;
  uint64_t code;
  const struct abbrev *abbrev;
  const char *ret;
  size_t i;

  if (offset < u->unit_data_offset ||
      offset - u->unit_data_offset >= u->unit_data_len) {
    error_callback(data, "abstract origin or specification out of range", 0);
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

  code = read_uleb128(&unit_buf);
  if (code == 0) {
    dwarf_buf_error(&unit_buf, "invalid abstract origin or specification", 0);
    return NULL;
  }

  abbrev = lookup_abbrev(&u->abbrevs, code, error_callback, data);
  if (abbrev == NULL) return NULL;

  ret = NULL;
  for (i = 0; i < abbrev->num_attrs; ++i) {
    struct attr_val val;

    if (!read_attribute(abbrev->attrs[i].form, abbrev->attrs[i].val, &unit_buf,
                        u->is_dwarf64, u->version, u->addrsize,
                        &ddata->dwarf_sections, ddata->altlink, &val))
      return NULL;

    switch (abbrev->attrs[i].name) {
      case DW_AT_name:

        if (ret != NULL) break;
        if (!resolve_string(&ddata->dwarf_sections, u->is_dwarf64,
                            ddata->is_bigendian, u->str_offsets_base, &val,
                            error_callback, data, &ret))
          return NULL;
        break;

      case DW_AT_linkage_name:
      case DW_AT_MIPS_linkage_name:

      {
        const char *s;

        s = NULL;
        if (!resolve_string(&ddata->dwarf_sections, u->is_dwarf64,
                            ddata->is_bigendian, u->str_offsets_base, &val,
                            error_callback, data, &s))
          return NULL;
        if (s != NULL) return s;
      } break;

      case DW_AT_specification:

      {
        const char *name;

        name = read_referenced_name_from_attr(ddata, u, &abbrev->attrs[i], &val,
                                              error_callback, data);
        if (name != NULL) ret = name;
      } break;

      default:
        break;
    }
  }

  return ret;
}

static int add_function_range(struct backtrace_state *state, void *rdata,
                              uint64_t lowpc, uint64_t highpc,
                              backtrace_error_callback error_callback,
                              void *data, void *pvec) {
  struct function *function = (struct function *)rdata;
  struct function_vector *vec = (struct function_vector *)pvec;
  struct function_addrs *p;

  if (vec->count > 0) {
    p = (struct function_addrs *)vec->vec.base + (vec->count - 1);
    if ((lowpc == p->high || lowpc == p->high + 1) && function == p->function) {
      if (highpc > p->high) p->high = highpc;
      return 1;
    }
  }

  p = ((struct function_addrs *)backtrace_vector_grow(
      state, sizeof(struct function_addrs), error_callback, data, &vec->vec));
  if (p == NULL) return 0;

  p->low = lowpc;
  p->high = highpc;
  p->function = function;

  ++vec->count;

  return 1;
}

static int read_function_entry(struct backtrace_state *state,
                               struct dwarf_data *ddata, struct unit *u,
                               uint64_t base, struct dwarf_buf *unit_buf,
                               const struct line_header *lhdr,
                               backtrace_error_callback error_callback,
                               void *data, struct function_vector *vec_function,
                               struct function_vector *vec_inlined) {
  while (unit_buf->left > 0) {
    uint64_t code;
    const struct abbrev *abbrev;
    int is_function;
    struct function *function;
    struct function_vector *vec;
    size_t i;
    struct pcrange pcrange;
    int have_linkage_name;

    code = read_uleb128(unit_buf);
    if (code == 0) return 1;

    abbrev = lookup_abbrev(&u->abbrevs, code, error_callback, data);
    if (abbrev == NULL) return 0;

    is_function = (abbrev->tag == DW_TAG_subprogram ||
                   abbrev->tag == DW_TAG_entry_point ||
                   abbrev->tag == DW_TAG_inlined_subroutine);

    if (abbrev->tag == DW_TAG_inlined_subroutine)
      vec = vec_inlined;
    else
      vec = vec_function;

    function = NULL;
    if (is_function) {
      function = ((struct function *)backtrace_alloc(state, sizeof *function,
                                                     error_callback, data));
      if (function == NULL) return 0;
      memset(function, 0, sizeof *function);
    }

    memset(&pcrange, 0, sizeof pcrange);
    have_linkage_name = 0;
    for (i = 0; i < abbrev->num_attrs; ++i) {
      struct attr_val val;

      if (!read_attribute(abbrev->attrs[i].form, abbrev->attrs[i].val, unit_buf,
                          u->is_dwarf64, u->version, u->addrsize,
                          &ddata->dwarf_sections, ddata->altlink, &val))
        return 0;

      if ((abbrev->tag == DW_TAG_compile_unit ||
           abbrev->tag == DW_TAG_skeleton_unit) &&
          abbrev->attrs[i].name == DW_AT_low_pc) {
        if (val.encoding == ATTR_VAL_ADDRESS)
          base = val.u.uint;
        else if (val.encoding == ATTR_VAL_ADDRESS_INDEX) {
          if (!resolve_addr_index(&ddata->dwarf_sections, u->addr_base,
                                  u->addrsize, ddata->is_bigendian, val.u.uint,
                                  error_callback, data, &base))
            return 0;
        }
      }

      if (is_function) {
        switch (abbrev->attrs[i].name) {
          case DW_AT_call_file:
            if (val.encoding == ATTR_VAL_UINT) {
              if (val.u.uint >= lhdr->filenames_count) {
                dwarf_buf_error(unit_buf,
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

            if (have_linkage_name) break;
            {
              const char *name;

              name = read_referenced_name_from_attr(ddata, u, &abbrev->attrs[i],
                                                    &val, error_callback, data);
              if (name != NULL) function->name = name;
            }
            break;

          case DW_AT_name:

            if (function->name != NULL) break;
            if (!resolve_string(&ddata->dwarf_sections, u->is_dwarf64,
                                ddata->is_bigendian, u->str_offsets_base, &val,
                                error_callback, data, &function->name))
              return 0;
            break;

          case DW_AT_linkage_name:
          case DW_AT_MIPS_linkage_name:

          {
            const char *s;

            s = NULL;
            if (!resolve_string(&ddata->dwarf_sections, u->is_dwarf64,
                                ddata->is_bigendian, u->str_offsets_base, &val,
                                error_callback, data, &s))
              return 0;
            if (s != NULL) {
              function->name = s;
              have_linkage_name = 1;
            }
          } break;

          case DW_AT_low_pc:
          case DW_AT_high_pc:
          case DW_AT_ranges:
            update_pcrange(&abbrev->attrs[i], &val, &pcrange);
            break;

          default:
            break;
        }
      }
    }

    if (is_function && function->name == NULL) {
      backtrace_free(state, function, sizeof *function, error_callback, data);
      is_function = 0;
    }

    if (is_function) {
      if (pcrange.have_ranges || (pcrange.have_lowpc && pcrange.have_highpc)) {
        if (!add_ranges(state, &ddata->dwarf_sections, ddata->base_address,
                        ddata->is_bigendian, u, base, &pcrange,
                        add_function_range, (void *)function, error_callback,
                        data, (void *)vec))
          return 0;
      } else {
        backtrace_free(state, function, sizeof *function, error_callback, data);
        is_function = 0;
      }
    }

    if (abbrev->has_children) {
      if (!is_function) {
        if (!read_function_entry(state, ddata, u, base, unit_buf, lhdr,
                                 error_callback, data, vec_function,
                                 vec_inlined))
          return 0;
      } else {
        struct function_vector fvec;

        memset(&fvec, 0, sizeof fvec);

        if (!read_function_entry(state, ddata, u, base, unit_buf, lhdr,
                                 error_callback, data, vec_function, &fvec))
          return 0;

        if (fvec.count > 0) {
          struct function_addrs *p;
          struct function_addrs *faddrs;

          p = ((struct function_addrs *)backtrace_vector_grow(
              state, sizeof(struct function_addrs), error_callback, data,
              &fvec.vec));
          if (p == NULL) return 0;
          p->low = 0;
          --p->low;
          p->high = p->low;
          p->function = NULL;

          if (!backtrace_vector_release(state, &fvec.vec, error_callback, data))
            return 0;

          faddrs = (struct function_addrs *)fvec.vec.base;
          backtrace_qsort(faddrs, fvec.count, sizeof(struct function_addrs),
                          function_addrs_compare);

          function->function_addrs = faddrs;
          function->function_addrs_count = fvec.count;
        }
      }
    }
  }

  return 1;
}

static void read_function_info(
    struct backtrace_state *state, struct dwarf_data *ddata,
    const struct line_header *lhdr, backtrace_error_callback error_callback,
    void *data, struct unit *u, struct function_vector *fvec,
    struct function_addrs **ret_addrs, size_t *ret_addrs_count) {
  struct function_vector lvec;
  struct function_vector *pfvec;
  struct dwarf_buf unit_buf;
  struct function_addrs *p;
  struct function_addrs *addrs;
  size_t addrs_count;

  if (fvec != NULL)
    pfvec = fvec;
  else {
    memset(&lvec, 0, sizeof lvec);
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

  while (unit_buf.left > 0) {
    if (!read_function_entry(state, ddata, u, 0, &unit_buf, lhdr,
                             error_callback, data, pfvec, pfvec))
      return;
  }

  if (pfvec->count == 0) return;

  p = ((struct function_addrs *)backtrace_vector_grow(
      state, sizeof(struct function_addrs), error_callback, data, &pfvec->vec));
  if (p == NULL) return;
  p->low = 0;
  --p->low;
  p->high = p->low;
  p->function = NULL;

  addrs_count = pfvec->count;

  if (fvec == NULL) {
    if (!backtrace_vector_release(state, &lvec.vec, error_callback, data))
      return;
    addrs = (struct function_addrs *)pfvec->vec.base;
  } else {
    addrs = ((struct function_addrs *)backtrace_vector_finish(
        state, &fvec->vec, error_callback, data));
    if (addrs == NULL) return;
    fvec->count = 0;
  }

  backtrace_qsort(addrs, addrs_count, sizeof(struct function_addrs),
                  function_addrs_compare);

  *ret_addrs = addrs;
  *ret_addrs_count = addrs_count;
}

static int report_inlined_functions(uintptr_t pc, struct function *function,
                                    backtrace_full_callback callback,
                                    void *data, const char **filename,
                                    int *lineno) {
  struct function_addrs *p;
  struct function_addrs *match;
  struct function *inlined;
  int ret;

  if (function->function_addrs_count == 0) return 0;

  if (pc + 1 == 0) return 0;

  p = ((struct function_addrs *)bsearch(
      &pc, function->function_addrs, function->function_addrs_count,
      sizeof(struct function_addrs), function_addrs_search));
  if (p == NULL) return 0;

  while (pc == (p + 1)->low) ++p;
  match = NULL;
  while (1) {
    if (pc < p->high) {
      match = p;
      break;
    }
    if (p == function->function_addrs) break;
    if ((p - 1)->low < p->low) break;
    --p;
  }
  if (match == NULL) return 0;

  inlined = match->function;

  ret = report_inlined_functions(pc, inlined, callback, data, filename, lineno);
  if (ret != 0) return ret;

  ret = callback(data, pc, *filename, *lineno, inlined->name);
  if (ret != 0) return ret;

  *filename = inlined->caller_filename;
  *lineno = inlined->caller_lineno;

  return 0;
}

static int dwarf_lookup_pc(struct backtrace_state *state,
                           struct dwarf_data *ddata, uintptr_t pc,
                           backtrace_full_callback callback,
                           backtrace_error_callback error_callback, void *data,
                           int *found) {
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

  entry = (ddata->addrs_count == 0 || pc + 1 == 0
               ? NULL
               : bsearch(&pc, ddata->addrs, ddata->addrs_count,
                         sizeof(struct unit_addrs), unit_addrs_search));

  if (entry == NULL) {
    *found = 0;
    return 0;
  }

  while (pc == (entry + 1)->low) ++entry;
  found_entry = 0;
  while (1) {
    if (pc < entry->high) {
      found_entry = 1;
      break;
    }
    if (entry == ddata->addrs) break;
    if ((entry - 1)->low < entry->low) break;
    --entry;
  }
  if (!found_entry) {
    *found = 0;
    return 0;
  }

  u = entry->u;
  lines = u->lines;

  while (entry > ddata->addrs && pc >= (entry - 1)->low &&
         pc < (entry - 1)->high) {
    if (state->threaded)
      lines = (struct line *)backtrace_atomic_load_pointer(&u->lines);

    if (lines != (struct line *)(uintptr_t)-1) break;

    --entry;

    u = entry->u;
    lines = u->lines;
  }

  if (state->threaded) lines = backtrace_atomic_load_pointer(&u->lines);

  new_data = 0;
  if (lines == NULL) {
    struct function_addrs *function_addrs;
    size_t function_addrs_count;
    struct line_header lhdr;
    size_t count;

    function_addrs = NULL;
    function_addrs_count = 0;
    if (read_line_info(state, ddata, error_callback, data, entry->u, &lhdr,
                       &lines, &count)) {
      struct function_vector *pfvec;

      if (state->threaded)
        pfvec = NULL;
      else
        pfvec = &ddata->fvec;
      read_function_info(state, ddata, &lhdr, error_callback, data, entry->u,
                         pfvec, &function_addrs, &function_addrs_count);
      free_line_header(state, &lhdr, error_callback, data);
      new_data = 1;
    }

    if (!state->threaded) {
      u->lines_count = count;
      u->function_addrs = function_addrs;
      u->function_addrs_count = function_addrs_count;
      u->lines = lines;
    } else {
      backtrace_atomic_store_size_t(&u->lines_count, count);
      backtrace_atomic_store_pointer(&u->function_addrs, function_addrs);
      backtrace_atomic_store_size_t(&u->function_addrs_count,
                                    function_addrs_count);
      backtrace_atomic_store_pointer(&u->lines, lines);
    }
  }

  if (lines == (struct line *)(uintptr_t)-1) {
    if (new_data)
      return dwarf_lookup_pc(state, ddata, pc, callback, error_callback, data,
                             found);
    return callback(data, pc, NULL, 0, NULL);
  }

  ln = (struct line *)bsearch(&pc, lines, entry->u->lines_count,
                              sizeof(struct line), line_search);
  if (ln == NULL) {
    if (entry->u->abs_filename == NULL) {
      const char *filename;

      filename = entry->u->filename;
      if (filename != NULL && !IS_ABSOLUTE_PATH(filename) &&
          entry->u->comp_dir != NULL) {
        size_t filename_len;
        const char *dir;
        size_t dir_len;
        char *s;

        filename_len = strlen(filename);
        dir = entry->u->comp_dir;
        dir_len = strlen(dir);
        s = (char *)backtrace_alloc(state, dir_len + filename_len + 2,
                                    error_callback, data);
        if (s == NULL) {
          *found = 0;
          return 0;
        }
        memcpy(s, dir, dir_len);

        s[dir_len] = '/';
        memcpy(s + dir_len + 1, filename, filename_len + 1);
        filename = s;
      }
      entry->u->abs_filename = filename;
    }

    return callback(data, pc, entry->u->abs_filename, 0, NULL);
  }

  if (entry->u->function_addrs_count == 0)
    return callback(data, pc, ln->filename, ln->lineno, NULL);

  p = ((struct function_addrs *)bsearch(
      &pc, entry->u->function_addrs, entry->u->function_addrs_count,
      sizeof(struct function_addrs), function_addrs_search));
  if (p == NULL) return callback(data, pc, ln->filename, ln->lineno, NULL);

  while (pc == (p + 1)->low) ++p;
  fmatch = NULL;
  while (1) {
    if (pc < p->high) {
      fmatch = p;
      break;
    }
    if (p == entry->u->function_addrs) break;
    if ((p - 1)->low < p->low) break;
    --p;
  }
  if (fmatch == NULL) return callback(data, pc, ln->filename, ln->lineno, NULL);

  function = fmatch->function;

  filename = ln->filename;
  lineno = ln->lineno;

  ret = report_inlined_functions(pc, function, callback, data, &filename,
                                 &lineno);
  if (ret != 0) return ret;

  return callback(data, pc, filename, lineno, function->name);
}

static int dwarf_fileline(struct backtrace_state *state, uintptr_t pc,
                          backtrace_full_callback callback,
                          backtrace_error_callback error_callback, void *data) {
  struct dwarf_data *ddata;
  int found;
  int ret;

  if (!state->threaded) {
    for (ddata = (struct dwarf_data *)state->fileline_data; ddata != NULL;
         ddata = ddata->next) {
      ret = dwarf_lookup_pc(state, ddata, pc, callback, error_callback, data,
                            &found);
      if (ret != 0 || found) return ret;
    }
  } else {
    struct dwarf_data **pp;

    pp = (struct dwarf_data **)(void *)&state->fileline_data;
    while (1) {
      ddata = backtrace_atomic_load_pointer(pp);
      if (ddata == NULL) break;

      ret = dwarf_lookup_pc(state, ddata, pc, callback, error_callback, data,
                            &found);
      if (ret != 0 || found) return ret;

      pp = &ddata->next;
    }
  }

  return callback(data, pc, NULL, 0, NULL);
}

static struct dwarf_data *build_dwarf_data(
    struct backtrace_state *state, uintptr_t base_address,
    const struct dwarf_sections *dwarf_sections, int is_bigendian,
    struct dwarf_data *altlink, backtrace_error_callback error_callback,
    void *data) {
  struct unit_addrs_vector addrs_vec;
  struct unit_addrs *addrs;
  size_t addrs_count;
  struct unit_vector units_vec;
  struct unit **units;
  size_t units_count;
  struct dwarf_data *fdata;

  if (!build_address_map(state, base_address, dwarf_sections, is_bigendian,
                         altlink, error_callback, data, &addrs_vec, &units_vec))
    return NULL;

  if (!backtrace_vector_release(state, &addrs_vec.vec, error_callback, data))
    return NULL;
  if (!backtrace_vector_release(state, &units_vec.vec, error_callback, data))
    return NULL;
  addrs = (struct unit_addrs *)addrs_vec.vec.base;
  units = (struct unit **)units_vec.vec.base;
  addrs_count = addrs_vec.count;
  units_count = units_vec.count;
  backtrace_qsort(addrs, addrs_count, sizeof(struct unit_addrs),
                  unit_addrs_compare);

  fdata = ((struct dwarf_data *)backtrace_alloc(
      state, sizeof(struct dwarf_data), error_callback, data));
  if (fdata == NULL) return NULL;

  fdata->next = NULL;
  fdata->altlink = altlink;
  fdata->base_address = base_address;
  fdata->addrs = addrs;
  fdata->addrs_count = addrs_count;
  fdata->units = units;
  fdata->units_count = units_count;
  fdata->dwarf_sections = *dwarf_sections;
  fdata->is_bigendian = is_bigendian;
  memset(&fdata->fvec, 0, sizeof fdata->fvec);

  return fdata;
}

int backtrace_dwarf_add(struct backtrace_state *state, uintptr_t base_address,
                        const struct dwarf_sections *dwarf_sections,
                        int is_bigendian, struct dwarf_data *fileline_altlink,
                        backtrace_error_callback error_callback, void *data,
                        fileline *fileline_fn,
                        struct dwarf_data **fileline_entry) {
  struct dwarf_data *fdata;

  fdata = build_dwarf_data(state, base_address, dwarf_sections, is_bigendian,
                           fileline_altlink, error_callback, data);
  if (fdata == NULL) return 0;

  if (fileline_entry != NULL) *fileline_entry = fdata;

  if (!state->threaded) {
    struct dwarf_data **pp;

    for (pp = (struct dwarf_data **)(void *)&state->fileline_data; *pp != NULL;
         pp = &(*pp)->next)
      ;
    *pp = fdata;
  } else {
    while (1) {
      struct dwarf_data **pp;

      pp = (struct dwarf_data **)(void *)&state->fileline_data;

      while (1) {
        struct dwarf_data *p;

        p = backtrace_atomic_load_pointer(pp);

        if (p == NULL) break;

        pp = &p->next;
      }

      if (__sync_bool_compare_and_swap(pp, NULL, fdata)) break;
    }
  }

  *fileline_fn = dwarf_fileline;

  return 1;
}

// fileline.c:
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#if defined(HAVE_KERN_PROC_ARGS) || defined(HAVE_KERN_PROC)
#include <sys/sysctl.h>
#endif

#ifdef HAVE_MACH_O_DYLD_H
#include <mach-o/dyld.h>
#endif

#ifndef HAVE_GETEXECNAME
#define getexecname() NULL
#endif

#if !defined(HAVE_KERN_PROC_ARGS) && !defined(HAVE_KERN_PROC)

#define sysctl_exec_name1(state, error_callback, data) NULL
#define sysctl_exec_name2(state, error_callback, data) NULL

#else
static char *sysctl_exec_name(struct backtrace_state *state, int mib0, int mib1,
                              int mib2, int mib3,
                              backtrace_error_callback error_callback,
                              void *data) {
  int mib[4];
  size_t len;
  char *name;
  size_t rlen;

  mib[0] = mib0;
  mib[1] = mib1;
  mib[2] = mib2;
  mib[3] = mib3;

  if (sysctl(mib, 4, NULL, &len, NULL, 0) < 0) return NULL;
  name = (char *)backtrace_alloc(state, len, error_callback, data);
  if (name == NULL) return NULL;
  rlen = len;
  if (sysctl(mib, 4, name, &rlen, NULL, 0) < 0) {
    backtrace_free(state, name, len, error_callback, data);
    return NULL;
  }
  return name;
}

#ifdef HAVE_KERN_PROC_ARGS

static char *sysctl_exec_name1(struct backtrace_state *state,
                               backtrace_error_callback error_callback,
                               void *data) {
  return sysctl_exec_name(state, CTL_KERN, KERN_PROC_ARGS, -1,
                          KERN_PROC_PATHNAME, error_callback, data);
}

#else

#define sysctl_exec_name1(state, error_callback, data) NULL

#endif

#ifdef HAVE_KERN_PROC

static char *sysctl_exec_name2(struct backtrace_state *state,
                               backtrace_error_callback error_callback,
                               void *data) {
  return sysctl_exec_name(state, CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1,
                          error_callback, data);
}

#else

#define sysctl_exec_name2(state, error_callback, data) NULL

#endif

#endif
#ifdef HAVE_MACH_O_DYLD_H

static char *macho_get_executable_path(struct backtrace_state *state,
                                       backtrace_error_callback error_callback,
                                       void *data) {
  uint32_t len;
  char *name;

  len = 0;
  if (_NSGetExecutablePath(NULL, &len) == 0) return NULL;
  name = (char *)backtrace_alloc(state, len, error_callback, data);
  if (name == NULL) return NULL;
  if (_NSGetExecutablePath(name, &len) != 0) {
    backtrace_free(state, name, len, error_callback, data);
    return NULL;
  }
  return name;
}

#else
#define macho_get_executable_path(state, error_callback, data) NULL

#endif

static int fileline_initialize(struct backtrace_state *state,
                               backtrace_error_callback error_callback,
                               void *data) {
  int failed;
  fileline fileline_fn;
  int pass;
  int called_error_callback;
  int descriptor;
  const char *filename;
  char buf[64];

  if (!state->threaded)
    failed = state->fileline_initialization_failed;
  else
    failed = backtrace_atomic_load_int(&state->fileline_initialization_failed);

  if (failed) {
    error_callback(data, "failed to read executable information", -1);
    return 0;
  }

  if (!state->threaded)
    fileline_fn = state->fileline_fn;
  else
    fileline_fn = backtrace_atomic_load_pointer(&state->fileline_fn);
  if (fileline_fn != NULL) return 1;

  descriptor = -1;
  called_error_callback = 0;
  for (pass = 0; pass < 8; ++pass) {
    int does_not_exist;

    switch (pass) {
      case 0:
        filename = state->filename;
        break;
      case 1:
        filename = getexecname();
        break;
      case 2:
        filename = "/proc/self/exe";
        break;
      case 3:
        filename = "/proc/curproc/file";
        break;
      case 4:
        snprintf(buf, sizeof(buf), "/proc/%ld/object/a.out", (long)getpid());
        filename = buf;
        break;
      case 5:
        filename = sysctl_exec_name1(state, error_callback, data);
        break;
      case 6:
        filename = sysctl_exec_name2(state, error_callback, data);
        break;
      case 7:
        filename = macho_get_executable_path(state, error_callback, data);
        break;
      default:
        abort();
    }

    if (filename == NULL) continue;

    descriptor =
        backtrace_open(filename, error_callback, data, &does_not_exist);
    if (descriptor < 0 && !does_not_exist) {
      called_error_callback = 1;
      break;
    }
    if (descriptor >= 0) break;
  }

  if (descriptor < 0) {
    if (!called_error_callback) {
      if (state->filename != NULL)
        error_callback(data, state->filename, ENOENT);
      else
        error_callback(data, "libbacktrace could not find executable to open",
                       0);
    }
    failed = 1;
  }

  if (!failed) {
    if (!backtrace_initialize(state, filename, descriptor, error_callback, data,
                              &fileline_fn))
      failed = 1;
  }

  if (failed) {
    if (!state->threaded)
      state->fileline_initialization_failed = 1;
    else
      backtrace_atomic_store_int(&state->fileline_initialization_failed, 1);
    return 0;
  }

  if (!state->threaded)
    state->fileline_fn = fileline_fn;
  else {
    backtrace_atomic_store_pointer(&state->fileline_fn, fileline_fn);
  }

  return 1;
}

int backtrace_pcinfo(struct backtrace_state *state, uintptr_t pc,
                     backtrace_full_callback callback,
                     backtrace_error_callback error_callback, void *data) {
  if (!fileline_initialize(state, error_callback, data)) return 0;

  if (state->fileline_initialization_failed) return 0;

  return state->fileline_fn(state, pc, callback, error_callback, data);
}

int backtrace_syminfo(struct backtrace_state *state, uintptr_t pc,
                      backtrace_syminfo_callback callback,
                      backtrace_error_callback error_callback, void *data) {
  if (!fileline_initialize(state, error_callback, data)) return 0;

  if (state->fileline_initialization_failed) return 0;

  state->syminfo_fn(state, pc, callback, error_callback, data);
  return 1;
}

void backtrace_syminfo_to_full_callback(void *data, uintptr_t pc,
                                        const char *symname,
                                        uintptr_t symval ATTRIBUTE_UNUSED,
                                        uintptr_t symsize ATTRIBUTE_UNUSED) {
  struct backtrace_call_full *bdata = (struct backtrace_call_full *)data;

  bdata->ret = bdata->full_callback(bdata->full_data, pc, NULL, 0, symname);
}

void backtrace_syminfo_to_full_error_callback(void *data, const char *msg,
                                              int errnum) {
  struct backtrace_call_full *bdata = (struct backtrace_call_full *)data;

  bdata->full_error_callback(bdata->full_data, msg, errnum);
}

// posix.c:
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
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

int backtrace_open(const char *filename,
                   backtrace_error_callback error_callback, void *data,
                   int *does_not_exist) {
  int descriptor;

  if (does_not_exist != NULL) *does_not_exist = 0;

  descriptor = open(filename, (int)(O_RDONLY | O_BINARY | O_CLOEXEC));
  if (descriptor < 0) {
    if (does_not_exist != NULL && (errno == ENOENT || errno == EACCES))
      *does_not_exist = 1;
    else
      error_callback(data, filename, errno);
    return -1;
  }

#ifdef HAVE_FCNTL

  fcntl(descriptor, F_SETFD, FD_CLOEXEC);
#endif

  return descriptor;
}

int backtrace_close(int descriptor, backtrace_error_callback error_callback,
                    void *data) {
  if (close(descriptor) < 0) {
    error_callback(data, "close", errno);
    return 0;
  }
  return 1;
}

// print.c:
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

struct print_data {
  struct backtrace_state *state;
  FILE *f;
};

static int print_callback(void *data, uintptr_t pc, const char *filename,
                          int lineno, const char *function) {
  struct print_data *pdata = (struct print_data *)data;

  fprintf(pdata->f, "0x%lx %s\n\t%s:%d\n", (unsigned long)pc,
          function == NULL ? "???" : function,
          filename == NULL ? "???" : filename, lineno);
  return 0;
}

static void error_callback(void *data, const char *msg, int errnum) {
  struct print_data *pdata = (struct print_data *)data;

  if (pdata->state->filename != NULL)
    fprintf(stderr, "%s: ", pdata->state->filename);
  fprintf(stderr, "libbacktrace: %s", msg);
  if (errnum > 0) fprintf(stderr, ": %s", strerror(errnum));
  fputc('\n', stderr);
}

void __attribute__((noinline))
backtrace_print(struct backtrace_state *state, int skip, FILE *f) {
  struct print_data data;

  data.state = state;
  data.f = f;
  backtrace_full(state, skip + 1, print_callback, error_callback,
                 (void *)&data);
}

// sort.c:
#include <stddef.h>
#include <sys/types.h>

static void swap(char *a, char *b, size_t size) {
  size_t i;

  for (i = 0; i < size; i++, a++, b++) {
    char t;

    t = *a;
    *a = *b;
    *b = t;
  }
}

void backtrace_qsort(void *basearg, size_t count, size_t size,
                     int (*compar)(const void *, const void *)) {
  char *base = (char *)basearg;
  size_t i;
  size_t mid;

tail_recurse:
  if (count < 2) return;

  swap(base, base + (count / 2) * size, size);

  mid = 0;
  for (i = 1; i < count; i++) {
    if ((*compar)(base, base + i * size) > 0) {
      ++mid;
      if (i != mid) swap(base + mid * size, base + i * size, size);
    }
  }

  if (mid > 0) swap(base, base + mid * size, size);

  if (2 * mid < count) {
    backtrace_qsort(base, mid, size, compar);
    base += (mid + 1) * size;
    count -= mid + 1;
    goto tail_recurse;
  } else {
    backtrace_qsort(base + (mid + 1) * size, count - (mid + 1), size, compar);
    count = mid;
    goto tail_recurse;
  }
}

// state.c:
#include <string.h>
#include <sys/types.h>

struct backtrace_state *backtrace_create_state(
    const char *filename, int threaded, backtrace_error_callback error_callback,
    void *data) {
  struct backtrace_state init_state;
  struct backtrace_state *state;

#ifndef HAVE_SYNC_FUNCTIONS
  if (threaded) {
    error_callback(data, "backtrace library does not support threads", 0);
    return NULL;
  }
#endif

  memset(&init_state, 0, sizeof init_state);
  init_state.filename = filename;
  init_state.threaded = threaded;

  state = ((struct backtrace_state *)backtrace_alloc(&init_state, sizeof *state,
                                                     error_callback, data));
  if (state == NULL) return NULL;
  *state = init_state;

  return state;
}

// backtrace.c:
#include <sys/types.h>

#ifdef BACKTRACE_SUPPORTED
#include <unwind.h>

struct backtrace_data {
  int skip;

  struct backtrace_state *state;

  backtrace_full_callback callback;

  backtrace_error_callback error_callback;

  void *data;

  int ret;

  int can_alloc;
};

static _Unwind_Reason_Code unwind(struct _Unwind_Context *context,
                                  void *vdata) {
  struct backtrace_data *bdata = (struct backtrace_data *)vdata;
  uintptr_t pc;
  int ip_before_insn = 0;

#ifdef HAVE_GETIPINFO
  pc = _Unwind_GetIPInfo(context, &ip_before_insn);
#else
  pc = _Unwind_GetIP(context);
#endif

  if (bdata->skip > 0) {
    --bdata->skip;
    return _URC_NO_REASON;
  }

  if (!ip_before_insn) --pc;

  if (!bdata->can_alloc)
    bdata->ret = bdata->callback(bdata->data, pc, NULL, 0, NULL);
  else
    bdata->ret = backtrace_pcinfo(bdata->state, pc, bdata->callback,
                                  bdata->error_callback, bdata->data);
  if (bdata->ret != 0) return _URC_END_OF_STACK;

  return _URC_NO_REASON;
}

int __attribute__((noinline))
backtrace_full(struct backtrace_state *state, int skip,
               backtrace_full_callback callback,
               backtrace_error_callback error_callback, void *data) {
  struct backtrace_data bdata;
  void *p;

  bdata.skip = skip + 1;
  bdata.state = state;
  bdata.callback = callback;
  bdata.error_callback = error_callback;
  bdata.data = data;
  bdata.ret = 0;

  p = backtrace_alloc(state, 4096, NULL, NULL);
  if (p == NULL)
    bdata.can_alloc = 0;
  else {
    backtrace_free(state, p, 4096, NULL, NULL);
    bdata.can_alloc = 1;
  }

  _Unwind_Backtrace(unwind, &bdata);
  return bdata.ret;
}
#else
// Copied from nounwind.c
int
backtrace_full (struct backtrace_state *state ATTRIBUTE_UNUSED,
		int skip ATTRIBUTE_UNUSED,
		backtrace_full_callback callback ATTRIBUTE_UNUSED,
		backtrace_error_callback error_callback, void *data)
{
  error_callback (data,
		  "no stack trace because unwind library not available",
		  0);
  return 0;
}
#endif

// simple.c:
#ifdef BACKTRACE_SUPPORTED
#include <unwind.h>

struct backtrace_simple_data {
  int skip;

  struct backtrace_state *state;

  backtrace_simple_callback callback;

  backtrace_error_callback error_callback;

  void *data;

  int ret;
};

static _Unwind_Reason_Code simple_unwind(struct _Unwind_Context *context,
                                         void *vdata) {
  struct backtrace_simple_data *bdata = (struct backtrace_simple_data *)vdata;
  uintptr_t pc;
  int ip_before_insn = 0;

#ifdef HAVE_GETIPINFO
  pc = _Unwind_GetIPInfo(context, &ip_before_insn);
#else
  pc = _Unwind_GetIP(context);
#endif

  if (bdata->skip > 0) {
    --bdata->skip;
    return _URC_NO_REASON;
  }

  if (!ip_before_insn) --pc;

  bdata->ret = bdata->callback(bdata->data, pc);

  if (bdata->ret != 0) return _URC_END_OF_STACK;

  return _URC_NO_REASON;
}

int __attribute__((noinline))
backtrace_simple(struct backtrace_state *state, int skip,
                 backtrace_simple_callback callback,
                 backtrace_error_callback error_callback, void *data) {
  struct backtrace_simple_data bdata;

  bdata.skip = skip + 1;
  bdata.state = state;
  bdata.callback = callback;
  bdata.error_callback = error_callback;
  bdata.data = data;
  bdata.ret = 0;
  _Unwind_Backtrace(simple_unwind, &bdata);
  return bdata.ret;
}
#else
int
backtrace_simple (struct backtrace_state *state ATTRIBUTE_UNUSED,
		  int skip ATTRIBUTE_UNUSED,
		  backtrace_simple_callback callback ATTRIBUTE_UNUSED,
		  backtrace_error_callback error_callback, void *data)
{
  error_callback (data,
		  "no stack trace because unwind library not available",
		  0);
  return 0;
}
#endif
