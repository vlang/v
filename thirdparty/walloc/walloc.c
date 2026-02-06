// walloc.c: a small malloc implementation for use in WebAssembly targets
// Copyright (c) 2020 Igalia, S.L.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

typedef __SIZE_TYPE__ size_t;
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __UINT8_TYPE__ uint8_t;

#define NULL ((void *)0)

#define STATIC_ASSERT_EQ(a, b) _Static_assert((a) == (b), "eq")

#ifndef NDEBUG
#define ASSERT(x)             \
    do                        \
    {                         \
        if (!(x))             \
            __builtin_trap(); \
    } while (0)
#else
#define ASSERT(x) \
    do            \
    {             \
    } while (0)
#endif
#define ASSERT_EQ(a, b) ASSERT((a) == (b))

static inline size_t max(size_t a, size_t b)
{
    return a < b ? b : a;
}
static inline uintptr_t align(uintptr_t val, uintptr_t alignment)
{
    return (val + alignment - 1) & ~(alignment - 1);
}
#define ASSERT_ALIGNED(x, y) ASSERT((x) == align((x), y))

#define CHUNK_SIZE 256
#define CHUNK_SIZE_LOG_2 8
#define CHUNK_MASK (CHUNK_SIZE - 1)
STATIC_ASSERT_EQ(CHUNK_SIZE, 1 << CHUNK_SIZE_LOG_2);

#define PAGE_SIZE 65536
#define PAGE_SIZE_LOG_2 16
#define PAGE_MASK (PAGE_SIZE - 1)
STATIC_ASSERT_EQ(PAGE_SIZE, 1 << PAGE_SIZE_LOG_2);

#define CHUNKS_PER_PAGE 256
STATIC_ASSERT_EQ(PAGE_SIZE, CHUNK_SIZE *CHUNKS_PER_PAGE);

#define GRANULE_SIZE 8
#define GRANULE_SIZE_LOG_2 3
#define LARGE_OBJECT_THRESHOLD 256
#define LARGE_OBJECT_GRANULE_THRESHOLD 32

STATIC_ASSERT_EQ(GRANULE_SIZE, 1 << GRANULE_SIZE_LOG_2);
STATIC_ASSERT_EQ(LARGE_OBJECT_THRESHOLD,
                 LARGE_OBJECT_GRANULE_THRESHOLD *GRANULE_SIZE);

struct chunk
{
    char data[CHUNK_SIZE];
};

// There are small object pages for allocations of these sizes.
#define FOR_EACH_SMALL_OBJECT_GRANULES(M) \
    M(1)                                  \
    M(2) M(3) M(4) M(5) M(6) M(8) M(10) M(16) M(32)

enum chunk_kind
{
#define DEFINE_SMALL_OBJECT_CHUNK_KIND(i) GRANULES_##i,
    FOR_EACH_SMALL_OBJECT_GRANULES(DEFINE_SMALL_OBJECT_CHUNK_KIND)
#undef DEFINE_SMALL_OBJECT_CHUNK_KIND

        SMALL_OBJECT_CHUNK_KINDS,
    FREE_LARGE_OBJECT = 254,
    LARGE_OBJECT = 255
};

static const uint8_t small_object_granule_sizes[] =
    {
#define SMALL_OBJECT_GRANULE_SIZE(i) i,
        FOR_EACH_SMALL_OBJECT_GRANULES(SMALL_OBJECT_GRANULE_SIZE)
#undef SMALL_OBJECT_GRANULE_SIZE
};

static enum chunk_kind granules_to_chunk_kind(unsigned granules)
{
#define TEST_GRANULE_SIZE(i) \
    if (granules <= i)       \
        return GRANULES_##i;
    FOR_EACH_SMALL_OBJECT_GRANULES(TEST_GRANULE_SIZE);
#undef TEST_GRANULE_SIZE
    return LARGE_OBJECT;
}

static unsigned chunk_kind_to_granules(enum chunk_kind kind)
{
    switch (kind)
    {
#define CHUNK_KIND_GRANULE_SIZE(i) \
    case GRANULES_##i:             \
        return i;
        FOR_EACH_SMALL_OBJECT_GRANULES(CHUNK_KIND_GRANULE_SIZE);
#undef CHUNK_KIND_GRANULE_SIZE
    default:
        return -1;
    }
}

// Given a pointer P returned by malloc(), we get a header pointer via
// P&~PAGE_MASK, and a chunk index via (P&PAGE_MASK)/CHUNKS_PER_PAGE.  If
// chunk_kinds[chunk_idx] is [FREE_]LARGE_OBJECT, then the pointer is a large
// object, otherwise the kind indicates the size in granules of the objects in
// the chunk.
struct page_header
{
    uint8_t chunk_kinds[CHUNKS_PER_PAGE];
};

struct page
{
    union
    {
        struct page_header header;
        struct chunk chunks[CHUNKS_PER_PAGE];
    };
};

#define PAGE_HEADER_SIZE (sizeof(struct page_header))
#define FIRST_ALLOCATABLE_CHUNK 1
STATIC_ASSERT_EQ(PAGE_HEADER_SIZE, FIRST_ALLOCATABLE_CHUNK *CHUNK_SIZE);

static struct page *get_page(void *ptr)
{
    return (struct page *)(char *)(((uintptr_t)ptr) & ~PAGE_MASK);
}
static unsigned get_chunk_index(void *ptr)
{
    return (((uintptr_t)ptr) & PAGE_MASK) / CHUNK_SIZE;
}

struct freelist
{
    struct freelist *next;
};

struct large_object
{
    struct large_object *next;
    size_t size;
};

#define LARGE_OBJECT_HEADER_SIZE (sizeof(struct large_object))

static inline void *get_large_object_payload(struct large_object *obj)
{
    return ((char *)obj) + LARGE_OBJECT_HEADER_SIZE;
}
static inline struct large_object *get_large_object(void *ptr)
{
    return (struct large_object *)(((char *)ptr) - LARGE_OBJECT_HEADER_SIZE);
}

static struct freelist *small_object_freelists[SMALL_OBJECT_CHUNK_KINDS];
static struct large_object *large_objects;

extern void __heap_base;
static size_t walloc_heap_size;

static struct page *
allocate_pages(size_t payload_size, size_t *n_allocated)
{
    size_t needed = payload_size + PAGE_HEADER_SIZE;
    size_t heap_size = __builtin_wasm_memory_size(0) * PAGE_SIZE;
    uintptr_t base = heap_size;
    uintptr_t preallocated = 0, grow = 0;

    if (!walloc_heap_size)
    {
        // We are allocating the initial pages, if any.  We skip the first 64 kB,
        // then take any additional space up to the memory size.
        uintptr_t heap_base = align((uintptr_t)&__heap_base, PAGE_SIZE);
        preallocated = heap_size - heap_base; // Preallocated pages.
        walloc_heap_size = preallocated;
        base -= preallocated;
    }

    if (preallocated < needed)
    {
        // Always grow the walloc heap at least by 50%.
        grow = align(max(walloc_heap_size / 2, needed - preallocated),
                     PAGE_SIZE);
        ASSERT(grow);
        if (__builtin_wasm_memory_grow(0, grow >> PAGE_SIZE_LOG_2) == -1)
        {
            return NULL;
        }
        walloc_heap_size += grow;
    }

    struct page *ret = (struct page *)base;
    size_t size = grow + preallocated;
    ASSERT(size);
    ASSERT_ALIGNED(size, PAGE_SIZE);
    *n_allocated = size / PAGE_SIZE;
    return ret;
}

static char *
allocate_chunk(struct page *page, unsigned idx, enum chunk_kind kind)
{
    page->header.chunk_kinds[idx] = kind;
    return page->chunks[idx].data;
}

// It's possible for splitting to produce a large object of size 248 (256 minus
// the header size) -- i.e. spanning a single chunk.  In that case, push the
// chunk back on the GRANULES_32 small object freelist.
static void maybe_repurpose_single_chunk_large_objects_head(void)
{
    if (large_objects->size < CHUNK_SIZE)
    {
        unsigned idx = get_chunk_index(large_objects);
        char *ptr = allocate_chunk(get_page(large_objects), idx, GRANULES_32);
        large_objects = large_objects->next;
        struct freelist *head = (struct freelist *)ptr;
        head->next = small_object_freelists[GRANULES_32];
        small_object_freelists[GRANULES_32] = head;
    }
}

// If there have been any large-object frees since the last large object
// allocation, go through the freelist and merge any adjacent objects.
static int pending_large_object_compact = 0;
static struct large_object **
maybe_merge_free_large_object(struct large_object **prev)
{
    struct large_object *obj = *prev;
    while (1)
    {
        char *end = get_large_object_payload(obj) + obj->size;
        ASSERT_ALIGNED((uintptr_t)end, CHUNK_SIZE);
        unsigned chunk = get_chunk_index(end);
        if (chunk < FIRST_ALLOCATABLE_CHUNK)
        {
            // Merging can't create a large object that newly spans the header chunk.
            // This check also catches the end-of-heap case.
            return prev;
        }
        struct page *page = get_page(end);
        if (page->header.chunk_kinds[chunk] != FREE_LARGE_OBJECT)
        {
            return prev;
        }
        struct large_object *next = (struct large_object *)end;

        struct large_object **prev_prev = &large_objects, *walk = large_objects;
        while (1)
        {
            ASSERT(walk);
            if (walk == next)
            {
                obj->size += LARGE_OBJECT_HEADER_SIZE + walk->size;
                *prev_prev = walk->next;
                if (prev == &walk->next)
                {
                    prev = prev_prev;
                }
                break;
            }
            prev_prev = &walk->next;
            walk = walk->next;
        }
    }
}
static void
maybe_compact_free_large_objects(void)
{
    if (pending_large_object_compact)
    {
        pending_large_object_compact = 0;
        struct large_object **prev = &large_objects;
        while (*prev)
        {
            prev = &(*maybe_merge_free_large_object(prev))->next;
        }
    }
}

// Allocate a large object with enough space for SIZE payload bytes.  Returns a
// large object with a header, aligned on a chunk boundary, whose payload size
// may be larger than SIZE, and whose total size (header included) is
// chunk-aligned.  Either a suitable allocation is found in the large object
// freelist, or we ask the OS for some more pages and treat those pages as a
// large object.  If the allocation fits in that large object and there's more
// than an aligned chunk's worth of data free at the end, the large object is
// split.
//
// The return value's corresponding chunk in the page as starting a large
// object.
static struct large_object *
allocate_large_object(size_t size)
{
    maybe_compact_free_large_objects();
    struct large_object *best = NULL, **best_prev = &large_objects;
    size_t best_size = -1;
    for (struct large_object **prev = &large_objects, *walk = large_objects;
         walk;
         prev = &walk->next, walk = walk->next)
    {
        if (walk->size >= size && walk->size < best_size)
        {
            best_size = walk->size;
            best = walk;
            best_prev = prev;
            if (best_size + LARGE_OBJECT_HEADER_SIZE == align(size + LARGE_OBJECT_HEADER_SIZE, CHUNK_SIZE))
                // Not going to do any better than this; just return it.
                break;
        }
    }

    if (!best)
    {
        // The large object freelist doesn't have an object big enough for this
        // allocation.  Allocate one or more pages from the OS, and treat that new
        // sequence of pages as a fresh large object.  It will be split if
        // necessary.
        size_t size_with_header = size + sizeof(struct large_object);
        size_t n_allocated = 0;
        struct page *page = allocate_pages(size_with_header, &n_allocated);
        if (!page)
        {
            return NULL;
        }
        char *ptr = allocate_chunk(page, FIRST_ALLOCATABLE_CHUNK, LARGE_OBJECT);
        best = (struct large_object *)ptr;
        size_t page_header = ptr - ((char *)page);
        best->next = large_objects;
        best->size = best_size =
            n_allocated * PAGE_SIZE - page_header - LARGE_OBJECT_HEADER_SIZE;
        ASSERT(best_size >= size_with_header);
    }

    allocate_chunk(get_page(best), get_chunk_index(best), LARGE_OBJECT);

    struct large_object *next = best->next;
    *best_prev = next;

    size_t tail_size = (best_size - size) & ~CHUNK_MASK;
    if (tail_size)
    {
        // The best-fitting object has 1 or more aligned chunks free after the
        // requested allocation; split the tail off into a fresh aligned object.
        struct page *start_page = get_page(best);
        char *start = get_large_object_payload(best);
        char *end = start + best_size;

        if (start_page == get_page(end - tail_size - 1))
        {
            // The allocation does not span a page boundary; yay.
            ASSERT_ALIGNED((uintptr_t)end, CHUNK_SIZE);
        }
        else if (size < PAGE_SIZE - LARGE_OBJECT_HEADER_SIZE - CHUNK_SIZE)
        {
            // If the allocation itself smaller than a page, split off the head, then
            // fall through to maybe split the tail.
            ASSERT_ALIGNED((uintptr_t)end, PAGE_SIZE);
            size_t first_page_size = PAGE_SIZE - (((uintptr_t)start) & PAGE_MASK);
            struct large_object *head = best;
            allocate_chunk(start_page, get_chunk_index(start), FREE_LARGE_OBJECT);
            head->size = first_page_size;
            head->next = large_objects;
            large_objects = head;

            maybe_repurpose_single_chunk_large_objects_head();

            struct page *next_page = start_page + 1;
            char *ptr = allocate_chunk(next_page, FIRST_ALLOCATABLE_CHUNK, LARGE_OBJECT);
            best = (struct large_object *)ptr;
            best->size = best_size = best_size - first_page_size - CHUNK_SIZE - LARGE_OBJECT_HEADER_SIZE;
            ASSERT(best_size >= size);
            start = get_large_object_payload(best);
            tail_size = (best_size - size) & ~CHUNK_MASK;
        }
        else
        {
            // A large object that spans more than one page will consume all of its
            // tail pages.  Therefore if the split traverses a page boundary, round up
            // to page size.
            ASSERT_ALIGNED((uintptr_t)end, PAGE_SIZE);
            size_t first_page_size = PAGE_SIZE - (((uintptr_t)start) & PAGE_MASK);
            size_t tail_pages_size = align(size - first_page_size, PAGE_SIZE);
            size = first_page_size + tail_pages_size;
            tail_size = best_size - size;
        }
        best->size -= tail_size;

        unsigned tail_idx = get_chunk_index(end - tail_size);
        while (tail_idx < FIRST_ALLOCATABLE_CHUNK && tail_size)
        {
            // We would be splitting in a page header; don't do that.
            tail_size -= CHUNK_SIZE;
            tail_idx++;
        }

        if (tail_size)
        {
            struct page *page = get_page(end - tail_size);
            char *tail_ptr = allocate_chunk(page, tail_idx, FREE_LARGE_OBJECT);
            struct large_object *tail = (struct large_object *)tail_ptr;
            tail->next = large_objects;
            tail->size = tail_size - LARGE_OBJECT_HEADER_SIZE;
            ASSERT_ALIGNED((uintptr_t)(get_large_object_payload(tail) + tail->size), CHUNK_SIZE);
            large_objects = tail;

            maybe_repurpose_single_chunk_large_objects_head();
        }
    }

    ASSERT_ALIGNED((uintptr_t)(get_large_object_payload(best) + best->size), CHUNK_SIZE);
    return best;
}

static struct freelist *
obtain_small_objects(enum chunk_kind kind)
{
    struct freelist **whole_chunk_freelist = &small_object_freelists[GRANULES_32];
    void *chunk;
    if (*whole_chunk_freelist)
    {
        chunk = *whole_chunk_freelist;
        *whole_chunk_freelist = (*whole_chunk_freelist)->next;
    }
    else
    {
        chunk = allocate_large_object(0);
        if (!chunk)
        {
            return NULL;
        }
    }
    char *ptr = allocate_chunk(get_page(chunk), get_chunk_index(chunk), kind);
    char *end = ptr + CHUNK_SIZE;
    struct freelist *next = NULL;
    size_t size = chunk_kind_to_granules(kind) * GRANULE_SIZE;
    for (size_t i = size; i <= CHUNK_SIZE; i += size)
    {
        struct freelist *head = (struct freelist *)(end - i);
        head->next = next;
        next = head;
    }
    return next;
}

static inline size_t size_to_granules(size_t size)
{
    return (size + GRANULE_SIZE - 1) >> GRANULE_SIZE_LOG_2;
}
static struct freelist **get_small_object_freelist(enum chunk_kind kind)
{
    ASSERT(kind < SMALL_OBJECT_CHUNK_KINDS);
    return &small_object_freelists[kind];
}

static void *
allocate_small(enum chunk_kind kind)
{
    struct freelist **loc = get_small_object_freelist(kind);
    if (!*loc)
    {
        struct freelist *freelist = obtain_small_objects(kind);
        if (!freelist)
        {
            return NULL;
        }
        *loc = freelist;
    }
    struct freelist *ret = *loc;
    *loc = ret->next;
    return (void *)ret;
}

static void *
allocate_large(size_t size)
{
    struct large_object *obj = allocate_large_object(size);
    return obj ? get_large_object_payload(obj) : NULL;
}

void *
malloc(size_t size)
{
    size_t granules = size_to_granules(size);
    enum chunk_kind kind = granules_to_chunk_kind(granules);
    return (kind == LARGE_OBJECT) ? allocate_large(size) : allocate_small(kind);
}

void free(void *ptr)
{
    if (!ptr)
        return;
    struct page *page = get_page(ptr);
    unsigned chunk = get_chunk_index(ptr);
    uint8_t kind = page->header.chunk_kinds[chunk];
    if (kind == LARGE_OBJECT)
    {
        struct large_object *obj = get_large_object(ptr);
        obj->next = large_objects;
        large_objects = obj;
        allocate_chunk(page, chunk, FREE_LARGE_OBJECT);
        pending_large_object_compact = 1;
    }
    else
    {
        size_t granules = kind;
        struct freelist **loc = get_small_object_freelist(granules);
        struct freelist *obj = ptr;
        obj->next = *loc;
        *loc = obj;
    }
}