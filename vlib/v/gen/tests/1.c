void foo(int a);
void User_inc_age(User u, int n);
int get_int(string a);
bool get_bool();
int get_int2();
void myuser();
multi_return_int_string multi_return();
void variadic(variadic_int a);
void ensure_cap(int required, int cap);
void println(string s);
void matches();
void end();
void localmod__pub_foo();
int localmod__get_int_10();
int pi = 3;
int pi2 = pi;

typedef struct {
	int age;
} User;

typedef enum {
	Color_red, // 0
	Color_green, // 1
	Color_blue, // 2
} Color;

int main() {
	int a = 10;
	a++;
	int negative = -a;
	2 < 3;
	a == 1;
	a++;
	foo(3);
	int ak = 10;
	int mypi = pi;
	Color color = Color_red;
	localmod__pub_foo();
	int ten = localmod__get_int_10();
	println(localmod__pub_int_const);
	int g = (int)(3.0);
	return 0;
}

void foo(int a) {
	while (true) {
	}
	for (int i = 0;
i < 10; i++) {
	}
	array_int nums = new_array_from_c_array(3, 3, sizeof(array_int), (void[]){
		1, 2, 3,
	});
	array_int nums2 = array_slice(nums, 0, 2);
	int number = nums[0];
	array_bool bools = new_array_from_c_array(2, 2, sizeof(array_bool), (void[]){
		true, false,
	});
	array_User users = new_array_from_c_array(1, 1, sizeof(array_User), (void[]){
		(User){
			},
	});
	bool b = bools[0];
	array_string mystrings = new_array_from_c_array(2, 2, sizeof(array_string), (void[]){
		tos3("a"), tos3("b"),
	});
	string s = mystrings[0];
	int x = 0;
	x = get_int2();
	int n = get_int2();
	bool q = true || false;
	bool b2 = bools[0] || true;
	bool b3 = get_bool() || true;
	int f = TODO_first(nums);
}

void User_inc_age(User u, int n) {
	u.age += n;
}

int get_int(string a) {
	return 10;
}

bool get_bool() {
	return true;
}

int get_int2() {
	string a = tos3("hello");
	return get_int(a);
}

void myuser() {
	int x = 1;
	int q = x | 4100;
	User user = (User){
		.age = 30,
	};
	int age = user.age + 1;
	int boo = 2;
	int boo2 = boo + 1;
	bool b = age > 0;
	bool b2 = user.age > 0;
	User user2 = (User){
		.age = 20,
	};
	user2.age = 20 + boo;
}

multi_return_int_string multi_return() {
	return (multi_return_int_string){.arg0=4,.arg1=tos3("four")};
}

void variadic(variadic_int a) {
	int x = path_sep;
	int y = true ? 1 : 0;
;
}

void ensure_cap(int required, int cap) {
	if (required < cap) {
		return;
	}
}

void println(string s) {
}

void matches() {
	int a = 100;
	int tmp1 = a;
	if tmp1 == 10{
		println(tos3("10"));

	}
	if tmp1 == 20{
		int k = a + 1;

	}
	;
}

int path_sep = 10;

void end() {
	int i = 2;
	int key = 10;
	bool x = i != -1 && key == 10;
	int e = 2 + 3 * 4;
}

int localmod__pub_int_const = 20;

void localmod__pub_foo() {
	int a = 10;
}

int localmod__get_int_10() {
	return 10;
}

