multi_return_int_string mr_test();
int testa();
string testb(int a);
int testc(int a);
int testa();
int testb();
int testa();

int main() {
    Bar b = (Bar){
        .a = 122,
    };
    Foo a = (Foo){
        .a = tos3("hello"),
        .b = b,
    };
    a.a = tos3("da");
    a.b.a = 111;
    string a1 = a.a;
    int a2 = ;
    int c = testa();
    c = 1;
    string d = testb(1);
    d = tos3("hello");
    string e = tos3("hello");
    e = testb(111);
    e = tos3("world");
    array_int f = new_array_from_c_array(4, 4, sizeof(array_int), {
        testa(), 2, 3, 4,
    });
    array_string g = new_array_from_c_array(2, 2, sizeof(array_string), {
        testb(1), tos3("hello"),
    });
	array_Foo arr_foo = new_array_from_c_array(1, 1, sizeof(array_Foo), {
		a,
	});
	Foo af_idx_el = arr_foo[0];
	string foo_a = af_idx_el.a;
	return 0;
}

multi_return_int_string mr_test() {
    return (multi_return_int_string){.arg0=1,.arg1=tos3("v")};
}

int testa() {
    return testc(1);
}

string testb(int a) {
    return tos3("hello");
}

int testc(int a) {
    return a;
}

int testa() {
    int a = ;
    a = 1;
    return 4;
}

int testb() {
    return 4;
}

int testa() {
    return 4;
}

typedef struct {
        int a;
} Bar;

typedef struct {
        string a;
        Bar b;
} Foo;
