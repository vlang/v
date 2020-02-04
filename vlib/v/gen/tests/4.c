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
return 0;
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
