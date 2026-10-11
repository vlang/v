// C objects that c_undeclared_identifier_test.c.v reads as `C.<name>` without
// declaring them in V.
static const char *v_undeclared_text = "hello from the header";
static const char *v_undeclared_build = __DATE__ " " __TIME__;
static const char v_undeclared_chars[] = "array text";
static int v_undeclared_count = 42;
static double v_undeclared_ratio = 1.5;

// Read only by a generic function, whose body is not checked.
static const char *v_undeclared_generic_text = "generic text";
static const char *v_undeclared_generic_name = "generic name";

struct v_undeclared_point {
	int x;
	int y;
};

static struct v_undeclared_point v_undeclared_origin = {3, 4};
