struct User {
	int age;
	string name;
};

// multi return structs
// end of definitions #endif
typedef Option Option_int;
Option_int get_opt();
void User_foo(User* u);

Option_int get_opt() {
	return opt_ok(& (int []) { 0 }, sizeof(int));
}

void User_foo(User* u) {
	int age = u->age;
	array_string zzz = array_repeat(new_array_from_c_array(1, 1, sizeof(string), (string[]){
tos3(""),
}), u->age);
}

int main() {
	User user = (User){
0};
	user.age = 10;
	user.age++;
	user.name = tos3("bob");
	Option_int n = get_opt();
	int a = /*opt*/(*(int*)n.data) + 3;
	return 0;
}
