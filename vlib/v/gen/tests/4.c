multi_return_int_string mr_test();
int testa();
string testb(int a);
int testc(int a);
int Foo_testa(Foo f);
int Foo_testb(Foo f);
int Bar_testa(Bar b);

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
    int a2 = TODO_testa(b);
    int c = testa();
    c = 1;
    string d = testb(1);
    d = tos3("hello");
    string e = tos3("hello");
    e = testb(111);
	e = tos3("world");
	array_int f = new_array_from_c_array(4, 4, sizeof(array_int), (void[]){
        testa(), 2, 3, 4,
	});
	array_string g = new_array_from_c_array(2, 2, sizeof(array_string), (void[]){
		testb(1), tos3("hello"),
	});
	array_Foo arr_foo = new_array_from_c_array(1, 1, sizeof(array_Foo), (void[]){
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

int Foo_testa(Foo f) {
    int a = TODO_testb(f);
    a = 1;
    return 4;
}

int Foo_testb(Foo f) {
    return 4;
}

int Bar_testa(Bar b) {
    return 4;
}

typedef struct {
        int a;
} Bar;

typedef struct {
        string a;
        Bar b;
} Foo;
