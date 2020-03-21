struct Bar {
	int a;
};

struct Foo {
	string a;
	Bar b;
};

// multi return structs
typedef struct {
	int arg0;
	string arg1;
} multi_return_int_string;

// end of definitions #endif
multi_return_int_string mr_test();
int testa();
string testb(int a);
int testc(int a);
int Foo_testa(Foo* f);
int Foo_testb(Foo* f);
int Bar_testa(Bar* b);

int main(int argc, char** argv) {
	_init();
	os__args = os__init_os_args(argc, (byteptr*)argv);
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
    int a2 = Bar_testa(&b);
    int c = testa();
    c = 1;
    string d = testb(1);
    d = tos3("hello");
    string e = tos3("hello");
    e = testb(111);
	e = tos3("world");
	array_int f = new_array_from_c_array(4, 4, sizeof(int), (int[]){
        testa(), 2, 3, 4,
	});
	array_string g = new_array_from_c_array(2, 2, sizeof(string), (string[]){
		testb(1), tos3("hello"),
	});
	array_Foo arr_foo = new_array_from_c_array(1, 1, sizeof(Foo), (Foo[]){
		a,
	});
	Foo af_idx_el = (*(Foo*)array_get(arr_foo, 0));
	string foo_a = af_idx_el.a;
    map_string_string m1 = new_map(1, sizeof(string));
    map_string_int m2 = new_map_init(2, sizeof(int), (string[2]){tos3("v"), tos3("lang"), }, (int[2]){1, 2, });
    string ma1 = tos3("hello");
    string ma2 = tos3("vlang");
    multi_return_int_string mr_578 = mr_test();
    int mr1 = mr_578.arg0;
    string mr2 = mr_578.arg1;
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

int Foo_testa(Foo* f) {
    int a = Foo_testb(f);
    a = 1;
    return 4;
}

int Foo_testb(Foo* f) {
    return 4;
}

int Bar_testa(Bar* b) {
    return 4;
}

void _init() {
}
