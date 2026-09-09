#ifndef V_NCURSES_HELPERS_H
#define V_NCURSES_HELPERS_H

#include <curses.h>

static inline WINDOW *v_ncurses_initscr(void) {
	return initscr();
}

static inline WINDOW *v_ncurses_stdscr(void) {
	return stdscr;
}

static inline int v_ncurses_endwin(void) {
	return endwin();
}

static inline int v_ncurses_cbreak(void) {
	return cbreak();
}

static inline int v_ncurses_nocbreak(void) {
	return nocbreak();
}

static inline int v_ncurses_raw(void) {
	return raw();
}

static inline int v_ncurses_noraw(void) {
	return noraw();
}

static inline int v_ncurses_echo(void) {
	return echo();
}

static inline int v_ncurses_noecho(void) {
	return noecho();
}

static inline int v_ncurses_keypad(void *win, int enabled) {
	return keypad((WINDOW *)win, enabled ? TRUE : FALSE);
}

static inline int v_ncurses_nodelay(void *win, int enabled) {
	return nodelay((WINDOW *)win, enabled ? TRUE : FALSE);
}

static inline void v_ncurses_timeout(int delay) {
	timeout(delay);
}

static inline void v_ncurses_wtimeout(void *win, int delay) {
	wtimeout((WINDOW *)win, delay);
}

static inline int v_ncurses_curs_set(int visibility) {
	return curs_set(visibility);
}

static inline int v_ncurses_clear(void) {
	return clear();
}

static inline int v_ncurses_refresh(void) {
	return refresh();
}

static inline int v_ncurses_getch(void) {
	return getch();
}

static inline int v_ncurses_addstr(const char *text) {
	return addnstr(text, -1);
}

static inline int v_ncurses_mvaddstr(int y, int x, const char *text) {
	return mvaddnstr(y, x, text, -1);
}

static inline WINDOW *v_ncurses_newwin(int lines, int cols, int begin_y, int begin_x) {
	return newwin(lines, cols, begin_y, begin_x);
}

static inline int v_ncurses_delwin(void *win) {
	return delwin((WINDOW *)win);
}

static inline int v_ncurses_box(void *win, unsigned int vertical, unsigned int horizontal) {
	return box((WINDOW *)win, (chtype)vertical, (chtype)horizontal);
}

static inline int v_ncurses_getmaxx(void *win) {
	return getmaxx((WINDOW *)win);
}

static inline int v_ncurses_getmaxy(void *win) {
	return getmaxy((WINDOW *)win);
}

static inline int v_ncurses_wrefresh(void *win) {
	return wrefresh((WINDOW *)win);
}

static inline int v_ncurses_wclear(void *win) {
	return wclear((WINDOW *)win);
}

static inline int v_ncurses_wgetch(void *win) {
	return wgetch((WINDOW *)win);
}

static inline int v_ncurses_wmove(void *win, int y, int x) {
	return wmove((WINDOW *)win, y, x);
}

static inline int v_ncurses_waddstr(void *win, const char *text) {
	return waddnstr((WINDOW *)win, text, -1);
}

static inline int v_ncurses_mvwaddstr(void *win, int y, int x, const char *text) {
	return mvwaddnstr((WINDOW *)win, y, x, text, -1);
}

static inline int v_ncurses_start_color(void) {
	return start_color();
}

static inline int v_ncurses_has_colors(void) {
	return has_colors() ? 1 : 0;
}

static inline int v_ncurses_init_pair(short pair, short fg, short bg) {
	return init_pair(pair, fg, bg);
}

static inline int v_ncurses_color_pair(int pair) {
	return COLOR_PAIR(pair);
}

static inline int v_ncurses_attron(int attr) {
	return attron(attr);
}

static inline int v_ncurses_attroff(int attr) {
	return attroff(attr);
}

static inline int v_ncurses_wattron(void *win, int attr) {
	return wattron((WINDOW *)win, attr);
}

static inline int v_ncurses_wattroff(void *win, int attr) {
	return wattroff((WINDOW *)win, attr);
}

static inline int v_ncurses_key_f(int n) {
	return KEY_F(n);
}

#endif
