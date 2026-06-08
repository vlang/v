#ifndef V_TRACK_HEAP_CHECKS_H
#define V_TRACK_HEAP_CHECKS_H

#if defined(CUSTOM_DEFINE_track_heap) && (defined(_VGCBOEHM) || defined(CUSTOM_DEFINE_gcboehm))
#error "-d track_heap requires manual memory management; rebuild with -gc none"
#endif

#if defined(CUSTOM_DEFINE_track_heap) && defined(CUSTOM_DEFINE_vgc)
#error "-d track_heap requires manual memory management; rebuild with -gc none"
#endif

#if defined(CUSTOM_DEFINE_track_heap) && defined(_VPREALLOC)
#error "-d track_heap requires manual memory management; rebuild with -gc none (not -prealloc)"
#endif

#endif
