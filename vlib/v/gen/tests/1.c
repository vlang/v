void foo(int a);
int get_int(string a);
int get_int2();
void myuser();
multi_return_int_string multi_return();
void variadic(variadic_int a);

typedef struct {
	int age;
} User;

int main() {
int a = 10;
a++;
int negative = -a;
a == 1;
foo(3);
int ak = 10;
return 0;
}

void foo(int a) {
	while (true) {

	}
	for (int i = 0;
i < 10; i++;
) {
	}
	array_int nums = new_array_from_c_array(3, 3, sizeof(array_int), {
		1, 2, 3,
	});
	int number = nums[0];
	int n = get_int2();
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
	int age = user.age + 1;
	int boo = 2;
	int boo2 = boo + 1;
	bool b = age > 0;
	bool b2 = user.age > 0;
}

multi_return_int_string multi_return() {
return (multi_return_int_string){.arg0=4,.arg1=tos3("four")};
} 

void variadic(variadic_int a) {
}
