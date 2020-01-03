void foo(int a);
int get_int(string a);
int get_int2();
void myuser();

typedef struct {
	int age;
} User;

int main() {
int a = 10;
a++;
int c = -a;
a == 1;
foo(3);
return 0;
}

void foo(int a) {
	void n = get_int2();
}

int get_int(string a) {
	return 10;
}

int get_int2() {
	string a = tos3("hello");
	return get_int(a);
}

void myuser() {
	User user = (User){
		.age = 10,
	};
	User age = user.age;
	bool b = age > 0;
}
