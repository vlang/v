struct IfExpr {
};

struct IntegerLiteral {
};

// Sum type
typedef struct {
	void* obj;
	int typ;
} Expr;

struct User {
	int age;
	string name;
};

// multi return structs
// end of definitions #endif
typedef Option Option_int;
Option_int get_opt();
void User_foo(User* u);
void println(string s);
void handle_expr(Expr e);
// TypeDecl

Option_int get_opt() {
	return opt_ok(& (int []) { 0 }, sizeof(int));
}

void User_foo(User* u) {
	int age = u->age;
	array_string zzz = array_repeat(new_array_from_c_array(1, 1, sizeof(string), (string[]){
tos3(""),
}), u->age);
}

void println(string s) {
}

void handle_expr(Expr e) {
	if (e.typ == _type_idx_IfExpr) {
		IfExpr* it = (IfExpr*)e.obj; // ST it
		println(tos3("if"));
	}
	else if (e.typ == _type_idx_IntegerLiteral) {
		IntegerLiteral* it = (IntegerLiteral*)e.obj; // ST it
		println(tos3("integer"));
	}
	else {
		println(tos3("else"));
	}
	;
}

int main() {
	User user = (User){
0};
	user.age = 10;
	user.age++;
	user.name = tos3("bob");
	Option_int n = get_opt();
	int a = /*opt*/(*(int*)n.data) + 3;
	handle_expr((IfExpr){
0});
	return 0;
}
