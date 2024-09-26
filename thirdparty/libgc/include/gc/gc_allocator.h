/*
 * Copyright (c) 1996-1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 2002
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/*
 * This implements standard-conforming allocators that interact with
 * the garbage collector.  Gc_allocator<T> allocates garbage-collectible
 * objects of type T.  Traceable_allocator<T> allocates objects that
 * are not themselves garbage collected, but are scanned by the
 * collector for pointers to collectible objects.  Traceable_alloc
 * should be used for explicitly managed STL containers that may
 * point to collectible objects.
 *
 * This code was derived from an earlier version of the GNU C++ standard
 * library, which itself was derived from the SGI STL implementation.
 *
 * Ignore-off-page allocator: George T. Talbot
 */

#ifndef GC_ALLOCATOR_H
#define GC_ALLOCATOR_H

#include "gc.h"

#include <new> // for placement new and bad_alloc

#ifdef GC_NAMESPACE_ALLOCATOR
namespace boehmgc
{
#endif

#if !defined(GC_NO_MEMBER_TEMPLATES) && defined(_MSC_VER) && _MSC_VER <= 1200
  // MSVC++ 6.0 do not support member templates.
# define GC_NO_MEMBER_TEMPLATES
#endif

#if defined(GC_NEW_ABORTS_ON_OOM) || defined(_LIBCPP_NO_EXCEPTIONS)
# define GC_ALLOCATOR_THROW_OR_ABORT() GC_abort_on_oom()
#else
# define GC_ALLOCATOR_THROW_OR_ABORT() throw std::bad_alloc()
#endif

#if __cplusplus >= 201103L
# define GC_ALCTR_PTRDIFF_T std::ptrdiff_t
# define GC_ALCTR_SIZE_T std::size_t
#else
# define GC_ALCTR_PTRDIFF_T ptrdiff_t
# define GC_ALCTR_SIZE_T size_t
#endif

// First some helpers to allow us to dispatch on whether or not a type
// is known to be pointer-free.  These are private, except that the client
// may invoke the GC_DECLARE_PTRFREE macro.

struct GC_true_type {};
struct GC_false_type {};

template <class GC_tp>
struct GC_type_traits {
  GC_false_type GC_is_ptr_free;
};

#define GC_DECLARE_PTRFREE(T) \
    template<> struct GC_type_traits<T> { GC_true_type GC_is_ptr_free; }

GC_DECLARE_PTRFREE(char);
GC_DECLARE_PTRFREE(signed char);
GC_DECLARE_PTRFREE(unsigned char);
GC_DECLARE_PTRFREE(signed short);
GC_DECLARE_PTRFREE(unsigned short);
GC_DECLARE_PTRFREE(signed int);
GC_DECLARE_PTRFREE(unsigned int);
GC_DECLARE_PTRFREE(signed long);
GC_DECLARE_PTRFREE(unsigned long);
GC_DECLARE_PTRFREE(float);
GC_DECLARE_PTRFREE(double);
GC_DECLARE_PTRFREE(long double);
// The client may want to add others.

// In the following GC_Tp is GC_true_type if we are allocating a pointer-free
// object.
template <class GC_Tp>
inline void * GC_selective_alloc(GC_ALCTR_SIZE_T n, GC_Tp,
                                 bool ignore_off_page) {
    void *obj = ignore_off_page ? GC_MALLOC_IGNORE_OFF_PAGE(n) : GC_MALLOC(n);
    if (0 == obj)
      GC_ALLOCATOR_THROW_OR_ABORT();
    return obj;
}

#if !defined(__WATCOMC__)
  // Note: template-id not supported in this context by Watcom compiler.
  template <>
  inline void * GC_selective_alloc<GC_true_type>(GC_ALCTR_SIZE_T n,
                                                 GC_true_type,
                                                 bool ignore_off_page) {
    void *obj = ignore_off_page ? GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(n)
                                 : GC_MALLOC_ATOMIC(n);
    if (0 == obj)
      GC_ALLOCATOR_THROW_OR_ABORT();
    return obj;
  }
#endif

// Now the public gc_allocator<T> class.
template <class GC_Tp>
class gc_allocator {
public:
  typedef GC_ALCTR_SIZE_T    size_type;
  typedef GC_ALCTR_PTRDIFF_T difference_type;
  typedef GC_Tp*       pointer;
  typedef const GC_Tp* const_pointer;
  typedef GC_Tp&       reference;
  typedef const GC_Tp& const_reference;
  typedef GC_Tp        value_type;

  template <class GC_Tp1> struct rebind {
    typedef gc_allocator<GC_Tp1> other;
  };

  GC_CONSTEXPR gc_allocator() GC_NOEXCEPT {}
  GC_CONSTEXPR gc_allocator(const gc_allocator&) GC_NOEXCEPT {}
# ifndef GC_NO_MEMBER_TEMPLATES
    template <class GC_Tp1> GC_ATTR_EXPLICIT
    GC_CONSTEXPR gc_allocator(const gc_allocator<GC_Tp1>&) GC_NOEXCEPT {}
# endif
  GC_CONSTEXPR ~gc_allocator() GC_NOEXCEPT {}

  GC_CONSTEXPR pointer address(reference GC_x) const { return &GC_x; }
  GC_CONSTEXPR const_pointer address(const_reference GC_x) const {
    return &GC_x;
  }

  // GC_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when GC_n == 0.
  GC_CONSTEXPR GC_Tp* allocate(size_type GC_n, const void* = 0) {
    GC_type_traits<GC_Tp> traits;
    return static_cast<GC_Tp *>(GC_selective_alloc(GC_n * sizeof(GC_Tp),
                                        traits.GC_is_ptr_free, false));
  }

  GC_CONSTEXPR void deallocate(pointer __p, size_type /* GC_n */) GC_NOEXCEPT {
    GC_FREE(__p);
  }

  GC_CONSTEXPR size_type max_size() const GC_NOEXCEPT {
    return static_cast<GC_ALCTR_SIZE_T>(-1) / sizeof(GC_Tp);
  }

  GC_CONSTEXPR void construct(pointer __p, const GC_Tp& __val) {
    new(__p) GC_Tp(__val);
  }

  GC_CONSTEXPR void destroy(pointer __p) { __p->~GC_Tp(); }
};

template<>
class gc_allocator<void> {
public:
  typedef GC_ALCTR_SIZE_T    size_type;
  typedef GC_ALCTR_PTRDIFF_T difference_type;
  typedef void*       pointer;
  typedef const void* const_pointer;
  typedef void        value_type;

  template <class GC_Tp1> struct rebind {
    typedef gc_allocator<GC_Tp1> other;
  };
};

template <class GC_T1, class GC_T2>
GC_CONSTEXPR inline bool operator==(const gc_allocator<GC_T1>&,
                                    const gc_allocator<GC_T2>&) GC_NOEXCEPT {
  return true;
}

template <class GC_T1, class GC_T2>
GC_CONSTEXPR inline bool operator!=(const gc_allocator<GC_T1>&,
                                    const gc_allocator<GC_T2>&) GC_NOEXCEPT {
  return false;
}

// Now the public gc_allocator_ignore_off_page<T> class.
template <class GC_Tp>
class gc_allocator_ignore_off_page {
public:
  typedef GC_ALCTR_SIZE_T    size_type;
  typedef GC_ALCTR_PTRDIFF_T difference_type;
  typedef GC_Tp*       pointer;
  typedef const GC_Tp* const_pointer;
  typedef GC_Tp&       reference;
  typedef const GC_Tp& const_reference;
  typedef GC_Tp        value_type;

  template <class GC_Tp1> struct rebind {
    typedef gc_allocator_ignore_off_page<GC_Tp1> other;
  };

  GC_CONSTEXPR gc_allocator_ignore_off_page() GC_NOEXCEPT {}
  GC_CONSTEXPR gc_allocator_ignore_off_page(
                const gc_allocator_ignore_off_page&) GC_NOEXCEPT {}
# ifndef GC_NO_MEMBER_TEMPLATES
    template <class GC_Tp1> GC_ATTR_EXPLICIT
    GC_CONSTEXPR gc_allocator_ignore_off_page(
                const gc_allocator_ignore_off_page<GC_Tp1>&) GC_NOEXCEPT {}
# endif
  GC_CONSTEXPR ~gc_allocator_ignore_off_page() GC_NOEXCEPT {}

  GC_CONSTEXPR pointer address(reference GC_x) const { return &GC_x; }
  GC_CONSTEXPR const_pointer address(const_reference GC_x) const {
    return &GC_x;
  }

  // GC_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when GC_n == 0.
  GC_CONSTEXPR GC_Tp* allocate(size_type GC_n, const void* = 0) {
    GC_type_traits<GC_Tp> traits;
    return static_cast<GC_Tp *>(GC_selective_alloc(GC_n * sizeof(GC_Tp),
                                        traits.GC_is_ptr_free, true));
  }

  GC_CONSTEXPR void deallocate(pointer __p, size_type /* GC_n */) GC_NOEXCEPT {
    GC_FREE(__p);
  }

  GC_CONSTEXPR size_type max_size() const GC_NOEXCEPT {
    return static_cast<GC_ALCTR_SIZE_T>(-1) / sizeof(GC_Tp);
  }

  GC_CONSTEXPR void construct(pointer __p, const GC_Tp& __val) {
    new(__p) GC_Tp(__val);
  }

  GC_CONSTEXPR void destroy(pointer __p) { __p->~GC_Tp(); }
};

template<>
class gc_allocator_ignore_off_page<void> {
public:
  typedef GC_ALCTR_SIZE_T    size_type;
  typedef GC_ALCTR_PTRDIFF_T difference_type;
  typedef void*       pointer;
  typedef const void* const_pointer;
  typedef void        value_type;

  template <class GC_Tp1> struct rebind {
    typedef gc_allocator_ignore_off_page<GC_Tp1> other;
  };
};

template <class GC_T1, class GC_T2>
GC_CONSTEXPR inline bool operator==(const gc_allocator_ignore_off_page<GC_T1>&,
                const gc_allocator_ignore_off_page<GC_T2>&) GC_NOEXCEPT {
  return true;
}

template <class GC_T1, class GC_T2>
GC_CONSTEXPR inline bool operator!=(const gc_allocator_ignore_off_page<GC_T1>&,
                const gc_allocator_ignore_off_page<GC_T2>&) GC_NOEXCEPT {
  return false;
}

// And the public traceable_allocator class.

// Note that we currently do not specialize the pointer-free case,
// since a pointer-free traceable container does not make that much sense,
// though it could become an issue due to abstraction boundaries.

template <class GC_Tp>
class traceable_allocator {
public:
  typedef GC_ALCTR_SIZE_T    size_type;
  typedef GC_ALCTR_PTRDIFF_T difference_type;
  typedef GC_Tp*       pointer;
  typedef const GC_Tp* const_pointer;
  typedef GC_Tp&       reference;
  typedef const GC_Tp& const_reference;
  typedef GC_Tp        value_type;

  template <class GC_Tp1> struct rebind {
    typedef traceable_allocator<GC_Tp1> other;
  };

  GC_CONSTEXPR traceable_allocator() GC_NOEXCEPT {}
  GC_CONSTEXPR traceable_allocator(const traceable_allocator&) GC_NOEXCEPT {}
# ifndef GC_NO_MEMBER_TEMPLATES
    template <class GC_Tp1> GC_ATTR_EXPLICIT
    GC_CONSTEXPR traceable_allocator(
                const traceable_allocator<GC_Tp1>&) GC_NOEXCEPT {}
# endif
  GC_CONSTEXPR ~traceable_allocator() GC_NOEXCEPT {}

  GC_CONSTEXPR pointer address(reference GC_x) const { return &GC_x; }
  GC_CONSTEXPR const_pointer address(const_reference GC_x) const {
    return &GC_x;
  }

  // GC_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when GC_n == 0.
  GC_CONSTEXPR GC_Tp* allocate(size_type GC_n, const void* = 0) {
    void * obj = GC_MALLOC_UNCOLLECTABLE(GC_n * sizeof(GC_Tp));
    if (0 == obj)
      GC_ALLOCATOR_THROW_OR_ABORT();
    return static_cast<GC_Tp*>(obj);
  }

  GC_CONSTEXPR void deallocate(pointer __p, size_type /* GC_n */) GC_NOEXCEPT {
    GC_FREE(__p);
  }

  GC_CONSTEXPR size_type max_size() const GC_NOEXCEPT {
    return static_cast<GC_ALCTR_SIZE_T>(-1) / sizeof(GC_Tp);
  }

  GC_CONSTEXPR void construct(pointer __p, const GC_Tp& __val) {
    new(__p) GC_Tp(__val);
  }

  GC_CONSTEXPR void destroy(pointer __p) { __p->~GC_Tp(); }
};

template<>
class traceable_allocator<void> {
public:
  typedef GC_ALCTR_SIZE_T    size_type;
  typedef GC_ALCTR_PTRDIFF_T difference_type;
  typedef void*       pointer;
  typedef const void* const_pointer;
  typedef void        value_type;

  template <class GC_Tp1> struct rebind {
    typedef traceable_allocator<GC_Tp1> other;
  };
};

template <class GC_T1, class GC_T2>
GC_CONSTEXPR inline bool operator==(const traceable_allocator<GC_T1>&,
                const traceable_allocator<GC_T2>&) GC_NOEXCEPT {
  return true;
}

template <class GC_T1, class GC_T2>
GC_CONSTEXPR inline bool operator!=(const traceable_allocator<GC_T1>&,
                const traceable_allocator<GC_T2>&) GC_NOEXCEPT {
  return false;
}

#undef GC_ALCTR_PTRDIFF_T
#undef GC_ALCTR_SIZE_T

#ifdef GC_NAMESPACE_ALLOCATOR
}
#endif

#endif /* GC_ALLOCATOR_H */
