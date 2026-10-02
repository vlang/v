// A C struct that has a tag and no typedef: C code must spell it `struct c_tag_point`.
struct c_tag_point {
	int x;
	int y;
};

static int c_tag_point_sum(struct c_tag_point *point) {
	return point->x + point->y;
}
