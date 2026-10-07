typedef struct { unsigned int row; unsigned int column; } VCallPoint;
static inline VCallPoint v_call_point(int x) {
    VCallPoint p = { (unsigned int)x, 2 };
    return p;
}
static inline VCallPoint v_call_point_from_point(VCallPoint p) { return p; }
static inline VCallPoint VCallPointFactory(int x) { return v_call_point(x); }
static inline VCallPoint* v_call_point_pointer(int x) {
    static VCallPoint p;
    p = v_call_point(x);
    return &p;
}
