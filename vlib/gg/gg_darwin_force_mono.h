#ifndef GG_DARWIN_FORCE_MONO_H
#define GG_DARWIN_FORCE_MONO_H

static int g_gg_force_mono = 0;

static inline void gg_set_force_mono(int on) {
	g_gg_force_mono = on;
}

static inline int gg_get_force_mono(void) {
	return g_gg_force_mono;
}

#endif