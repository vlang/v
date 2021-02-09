# Documentación de V

## Introducción

V es un lenguaje de programación compilado de tipado estático diseñado para
crear software mantenible.

Es similar a Go y su diseño también ha sido influenciado por Oberon, Rust, Swift,
Kotlin y Python.

V es un lenguaje muy simple. Leer esta documentación te llevará aproximadamente una hora,
y al final habrás aprendido prácticamente todo el lenguaje.

El lenguaje promueve la escritura de código simple y claro con mínima abstracción.

A pesar de ser simple, V le da mucho poder al desarrollador.
Todo lo que puedas hacer en otros lenguajes, lo puedes hacer en V.

## Instalación desde el código fuente

La forma principal de obtener la versión de V más reciente y mejorada es
__instalarlo desde el código fuente__.
Es __fácil__ y, por lo general, toma __sólo unos segundos__.

### Linux, macOS, FreeBSD, etc:

Solo necesitas `git`, un compilador de C como `gcc` o `clang`, y `make`:

```bash
git clone https://github.com/vlang/v && cd v && make
```

### Windows

Solo necesitas `git`, y un compilador de C como `gcc` o `msvc`:

```bash
git clone https://github.com/vlang/v
cd v
make
```

### Android

También es posible ejecutar aplicaciones gráficas hechas con V en Android
a través de [vab](https://github.com/vlang/vab).

Las dependencias que necesita V en Android son: **V** (obviamente), **JavaSDK** >= 8
y Android **SDK + NDK**.

  1. Instale las dependencias (mirar [vab](https://github.com/vlang/vab))
  2. Conecte tu dispositivo Android a la PC
  3. Ejecute:
  ```bash
  git clone https://github.com/vlang/vab && cd vab && v vab.v
  ./vab --device auto run /path/to/v/examples/sokol/particles
  ```

Para más detalles y solución de problemas, por favor visite el repositorio en Github de
[vab](https://github.com/vlang/vab).

## Tabla de contenidos

<table>
    <tr><td width=33% valign=top>

* [Hola Mundo](#hola-mundo)
* [Comentarios](#comentarios)
* [Funciones](#funciones)
    * [Retornando múltiples valores](#retornando-m%C3%BAltiples-valores)
    * [Número variable de argumentos](#n%C3%BAmero-variable-de-argumentos)
* [Visibilidad de un símbolo](#visibilidad-de-un-s%C3%ADmbolo)
* [Variables](#variables)
    * [Variables mutables](#variables-mutables)
    * [Inicialización vs asignamiento](#inicializaci%C3%B3n-vs-asignamiento)
    * [Errores de declaración](#errores-de-declaraci%C3%B3n)
* [Tipos](#tipos)
    * [Tipos primitivos](#tipos-primitivos)
    * [Strings (Cadenas)](#strings-cadenas)
    * [Números](#n%C3%BAmeros)
    * [Matrices (Arrays)](#matrices-arrays)
    * [Matrices de tamaño fijo](#matrices-de-tamaño-fijo)
    * [Mapas](#mapas)
* [Importación de módulos](#importaci%C3%B3n-de-m%C3%B3dulos)
</td></tr>
</table>

## Hola Mundo

```v
fn main() {
	println('hola mundo')
}
```

Guarda este fragmento de código en un archivo llamado "hello.v". Ahora ejecuta: `v run hello.v`.

> Esto se hace asumiendo que ha enlazado V usando `v symlink`, como se describe
[aquí (en inglés)](https://github.com/vlang/v/blob/master/README.md#symlinking).
Si aún no lo has hecho, debes escribir la ruta al ejecutable de V manualmente.

¡Felicitaciones, acabas de escribir y ejecutar tu primer programa escrito en V!

También puedes compilar un programa sin ejecutarlo con `v hello.v`.
Puedes ejecutar `v help` para ver todos los comandos soportados.

En el ejemplo anterior, puede ver que las funciones se declaran con la palabra clave `fn`.
El tipo de retorno se especifica después del nombre de la función.
En este caso, `main` no devuelve nada, por lo que no hay ningún tipo de retorno.

Como en muchos otros lenguajes (como C, Go y Rust), `main` es el punto de entrada de su programa.

`println` es una de las pocas funciones integradas.
Imprime el valor que se le ha pasado a la salida estándar.

La declaración `fn main()` se puede omitir en programas que se componen de un solo archivo.
Esto es útil al escribir pequeños programas, "scripts" o simplemente aprender el lenguaje.
Por brevedad, `fn main()` se omitirá en este tutorial.

Esto significa que un programa de "hola mundo" en V es tan simple como

```v
println('hello world')
```

## Comentarios

```v
// Esto es un comentario de una simple línea.
/*
Esto es un comentario multilínea.
   /* Y este tipo de comentario puede ir dentro de otro. */
*/
```

## Funciones

```v
fn main() {
	println(add(77, 33))
	println(sub(100, 50))
}

fn add(x int, y int) int {
	return x + y
}

fn sub(x int, y int) int {
	return x - y
}
```

Nuevamente, el tipo viene después del nombre del argumento.

Al igual que en Go y C, las funciones no se pueden sobrecargar.
Esto simplifica el código y mejora la facilidad de mantenimiento y la legibilidad.

Las funciones se pueden utilizar antes de su declaración:
`add` y `sub` se declaran después de `main`, pero aún se pueden llamar desde` main`.
Esto es cierto para todas las declaraciones en V y elimina la necesidad de archivos
de encabezado o el pensar en el orden de archivos y declaraciones.

### Retornando múltiples valores

```v
fn foo() (int, int) {
	return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
c, _ := foo() // puedes ignorar valores usando la variable `_`
```

### Número variable de argumentos

```v
fn sum(a ...int) int {
	mut total := 0
	for x in a {
		total += x
	}
	return total
}

println(sum()) // 0
println(sum(1)) // 1
println(sum(2, 3)) // 5

// usando la descomposición de arrays (matrices)
a := [2, 3, 4]
println(sum(...a)) // <-- usando el prefijo ... aquí. se imprime: 9
b := [5, 6, 7]
println(sum(...b)) // se imprime: 18
```

## Visibilidad de un símbolo

```v
pub fn public_function() {
}

fn private_function() {
}
```

Las funciones son privadas (o, no exportadas) de forma predeterminada.
Para permitir que otros módulos los usen, tienes que anteponer `pub`. Lo mismo aplica
a constantes y tipos (structs, enums, type, etc).

Nota: `pub` solo se puede usar desde un módulo con nombre.
Para obtener información sobre cómo crear un módulo, puedes consultar [Módulos](#modulos).

## Variables

```v
name := 'Bob'
age := 20
large_number := i64(9999999999)
println(name)
println(age)
println(large_number)
```

Las variables se declaran e inicializan con `:=`. Esta es la única
forma de declarar variables en V. Esto significa que las variables siempre tienen un valor
inicial.

El tipo de variable se infiere del valor del lado derecho.
Para elegir un tipo diferente, puedes usar la conversión de tipos:
la expresión `T(v)` convierte el valor `v` al
tipo `T`.

A diferencia de la mayoría de los otros lenguajes, V solo permite definir variables en funciones.
No se permiten variables globales (a nivel de módulo). No hay un estado global en V
(ver [Funciones puras por defecto](#funciones-puras-por-defecto) para más detalles).

Para lograr coherencia en diferentes bases de código, todos los nombres de funciones y variables
deben usar el estilo `snake_case`, a diferencia de los nombres de tipo, que deben usar `PascalCase`.

### Variables mutables

```v
mut age := 20
println(age)
age = 21
println(age)
```

Para cambiar el valor de una variable usa `=`. En V, las variables son
inmutables por defecto.
Para poder cambiar el valor de la variable, debes declararlo con `mut`.

Intenta compilar el programa anterior después de eliminar el `mut` de la primera línea.

### Inicialización vs asignamiento

Ten en cuenta la diferencia (importante) entre `:=` y `=`.
`:=` se usa para declarar e inicializar, `=` se usa para asignar.

```v failcompile
fn main() {
    age = 21
}
```

Este código no se compilará porque la variable `edad` no está declarada.
Todas las variables deben declararse en V.

```v
fn main() {
	age := 21
}
```

Los valores de múltiples variables se pueden cambiar en una línea.
De esta forma, sus valores se pueden intercambiar sin una variable intermedia.

```v
mut a := 0
mut b := 1
println('$a, $b') // 0, 1
a, b = b, a
println('$a, $b') // 1, 0
```

### Errores de declaración

En el modo de desarrollo, el compilador te advertirá que no has utilizado la variable
(obtendrás una advertencia: "unused variable", variable no utilizada).
En modo de producción (habilitado al pasar la opción `-prod` a v – `v -prod foo.v`)
no se compilará en absoluto (como en Go).

```v failcompile
fn main() {
    a := 10
    if true {
        a := 20 // error: redefinition of `a`
    }
    // warning: unused variable `a`
}
```

A diferencia de la mayoría de los lenguajes, no se permite el shadowing (ocultamiento) de
variables. Declarar una variable con un nombre que ya se utiliza en un ámbito pariente
provocará un error de compilación.

Sin embargo, puedes ocultar módulos importados, ya que es muy útil en algunas situaciones:

```v ignore
import ui
import gg

fn draw(ctx &gg.Context) {
    gg := ctx.parent.get_ui().gg
    gg.draw_rect(10, 10, 100, 50)
}
```

## Tipos

### Tipos primitivos

```v ignore
bool

string

i8    i16  int  i64      i128 (soon)
byte  u16  u32  u64      u128 (soon)

rune // representa un punto de código Unicode

f32 f64

byteptr, voidptr, charptr, size_t // estos se utilizan principalmente para la interoperabilidad de C

any // similar al void* de C, y el interface{} de Go
```

Ten en cuenta que, a diferencia de C y Go, `int` es siempre un número entero de 32 bits.

Existe una excepción a la regla de que todos los operadores
en V deben tener valores del mismo tipo en ambos lados. Un pequeño tipo primitivo
en un lado se puede promover automáticamente si encaja
completamente en el rango de datos del tipo en el otro lado.
Estas son las posibilidades permitidas:

```v ignore
   i8 → i16 → int → i64
                  ↘     ↘
                    f32 → f64
                  ↗     ↗
 byte → u16 → u32 → u64 ⬎
      ↘     ↘     ↘      ptr
   i8 → i16 → int → i64 ⬏
```

Un valor `int`, por ejemplo, se puede promover automáticamente a `f64`
o `i64` pero no a `f32` o `u32`. (`f32` significaría precisión
pérdida para valores grandes y `u32` significaría la pérdida del signo para
valores negativos).

Los literales como `123` o `4.56` se tratan de una manera especial. No conducen
a promociones de tipo, sin embargo, por defecto son `int` y` f64` respectivamente,
cuando se debe decidir su tipo:

```v ignore
u := u16(12)
v := 13 + u    // v es de tipo `u16` - no hay promoción
x := f32(45.6)
y := x + 3.14  // x es de tipo `f32` - no hay promoción
a := 75        // a es de tipo `int` - predeterminado para un literal de int (enteros)
b := 14.7      // b es de tipo `f64` - predeterminado para un literal de float (decimales)
c := u + a     // c es de tipo `int` - promoción automática del valor de `u`
d := b + x     // d es de tipo `f64` - promoción automática del valor de `x`
```

### Strings (Cadenas)

```v
name := 'Bob'
println(name.len)
println(name[0]) // la indexación da un byte: 'B'
println(name[1..3]) // el slicing (corte) da una cadena 'ob'
windows_newline := '\r\n' // asi se escapan caracteres especiales como en C
assert windows_newline.len == 2
```

En V, una cadena (string) es una matriz (array) de bytes
de sólo lectura. Los datos de las cadenas se codifican utilizando UTF-8.
Los valores de las cadenas son inmutables. No se pueden mutar los elementos:

```v failcompile
mut s := 'hello 🌎'
s[0] = `H` // esto no está permitido
```

> error: cannot assign to `s[i]` since V strings are immutable

Ten en cuenta que la indexación de una cadena producirá un `byte`,
no un `rune`. Los índices corresponden
a los bytes de la cadena, no a los puntos de código Unicode.

Los literales de caracteres tienen el tipo "rune". Para denotarlas, utilice un ` (backtick)

```v
rocket := `🚀`
assert 'aloha!'[0] == `a`
```

Se pueden utilizar tanto comillas simples como dobles para denotar cadenas. Por coherencia,
`vfmt` convierte las comillas dobles en comillas simples a menos
que la cadena contenga un carácter de comillas simples.

Para las cadenas sin procesar (raw strings), puedes antepoer un `r`.
Las cadenas sin procesar no se escapan:

```v
s := r'hello\nworld'
println(s) // "hello\nworld"
```

Las cadenas se pueden convertir fácilmente en números enteros (integers):

```v
s := '42'
n := s.int() // 42
```

### Interpolación de cadenas (String interpolation)

La sintaxis básica de la interpolación es bastante sencilla: utilice
un `$` antes de un nombre de variable.
La variable se convertirá en una cadena y se incrustará en el literal:

```v
name := 'Bob'
println('Hello, $name!') // Hello, Bob!
```

También funciona con los campos: `'edad = $usuario.edad'`.
Si necesitas expresiones más complejas, utiliza `${}`: `'puede registrarse = ${user.age > 13}'`.

También se admiten especificadores de formato similares a los de `printf()` en C.
`f`, `g`, `x`, etc. son opcionales y especifican el formato de salida.
El compilador se encarga del tamaño de almacenamiento, por lo que no hay necesidad de `hd` o `llu`.

```v
x := 123.4567
println('x = ${x:4.2f}')
println('[${x:10}]') // espacios a la izquierda => [   123.457]
println('[${int(x):-10}]') // espacios a la derecha => [123       ]
println('[${int(x):010}]') // ceros a la izquierda => [0000000123]
```

### Operadores de cadena (String operators)

```v
name := 'Bob'
bobby := name + 'by' // + es usado para concatenar cadenas
println(bobby) // "Bobby"
mut s := 'hello '
s += 'world' // `+=` se utiliza para añadir una cadena a otra
println(s) // "hello world"
```

Todos los operadores en V deben tener valores del mismo tipo en ambos lados.
No se puede concatenar un entero con una cadena:

```v failcompile
age := 10
println('age = ' + age) // not allowed
```
> error: infix expr: cannot use `int` (right expression) as `string`

Tenemos que convertir a `edad` en una `cadena`:

```v
age := 11
println('age = ' + age.str())
```

o utilizar la interpolación de cadenas (el más preferido):

```v
age := 12
println('age = $age')
```

### Números

```v
a := 123
```

Esto asignará el valor de 123 a "a". Por defecto `a` tendrá el tipo `int`.

También puedes utilizar la notación hexadecimal, binaria u octal para los literales enteros:

```v
a := 0x7B
b := 0b01111011
c := 0o173
```

A todos ellos se les asignará el mismo valor, 123. Todos ellos tendrán el tipo
`int`, sin importar la notación que hayas utilizado.

V también permite escribir números usando `_` (underscore, guión bajo) como separador:

```v
num := 1_000_000 // igual que escribir: 1000000
three := 0b0_11 // igual que escribir: 0b11
float_num := 3_122.55 // igual que escribir: 3122.55
hexa := 0xF_F // igual que escribir: 255
oct := 0o17_3 // igual que escribir: 0o173
```

Si quieres un tipo de entero diferente, puedes usar el casting:

```v
a := i64(123)
b := byte(42)
c := i16(12345)
```

La asignación de números en coma flotante funciona de la misma manera:

```v
f := 1.0
f1 := f64(3.14)
f2 := f32(3.14)
```

Si no se especifica el tipo explícitamente, por defecto los literales float
tendrán el tipo `f64`.

### Matrices (Arrays)

```v
mut nums := [1, 2, 3]
println(nums) // "[1, 2, 3]"
println(nums[1]) // "2"
nums[1] = 5
println(nums) // "[1, 5, 3]"
println(nums.len) // "3"
nums = [] // La matriz está vacía ahora
println(nums.len) // "0"
// Declarando una matriz vacía:
users := []int{}
```

El tipo de una matriz viene determinado por el primer elemento:

* `[1, 2, 3]` es una matriz de enteros (`[]int`).
* `['a', 'b']` es una matriz de cadenas (`[]string`).

Puedes especificar explícitamente el tipo del primer elemento
haciendo un casting: `[byte(16), 32, 64, 128]`.
Las matrices en V son homogéneos (todos los elementos deben tener del mismo tipo).
Esto significa que un código como `[1, 'a']` no se compilará.

El campo `.len` devuelve la longitud de la matriz. Ten en cuenta que es un campo de sólo lectura
y no puede ser modificado. Los campos exportados son de sólo lectura por defecto en V.
Ver [Modificadores de acceso](#modificadores-de-acceso).

#### Operaciones con las matrices

```v
mut nums := [1, 2, 3]
nums << 4
println(nums) // "[1, 2, 3, 4]"
// añadir a la matriz nuevos elementos desde otra matriz
nums << [5, 6, 7]
println(nums) // "[1, 2, 3, 4, 5, 6, 7]"
mut names := ['John']
names << 'Peter'
names << 'Sam'
// names << 10  <-- Esto no compilará. `names` es una matriz de cadenas.
println(names.len) // "3"
println('Alex' in names) // "false"
```

`<<` es un operador que añade un valor al final de la matriz.
También puedes añadir una matriz completa.

`val in array` devuelve true (verdadero) si la matriz contiene
el elemento `val`. Ver [operador `in`](#operador-in).

#### Inicialización de las propiedades de una matriz

Durante la inicialización puedes especificar la capacidad del
array (`cap`), su longitud inicial (`len`)
y el elemento por defecto (`init`):

```v
arr := []int{len: 5, init: -1}
// `[-1, -1, -1, -1, -1]`
```

El ajuste de la capacidad mejora el rendimiento de las inserciones
ya que reduce el número de reasignaciones necesarias:

```v
mut numbers := []int{cap: 1000}
println(numbers.len) // 0
// Ahora al añadir elementos no se reasignan
for i in 0 .. 1000 {
	numbers << i
}
```

Nota: El código anterior utiliza una sentencia [range `for`](#range-for).

#### Métodos de matrices

Todas las matrices pueden imprimirse fácilmente con `println(arr)` y convertirse en una cadena
con `s := arr.str()`.

La copia de los datos de la matriz se realiza con `.clone()`:

```v
nums := [1, 2, 3]
nums_copy := nums.clone()
```

Las matrices pueden ser filtradas y mapeadas eficientemente con los métodos `.filter()` y
`.map()`:

```v
nums := [1, 2, 3, 4, 5, 6]
even := nums.filter(it % 2 == 0)
println(even) // [2, 4, 6]
// filter puede aceptar funciones anónimas
even_fn := nums.filter(fn (x int) bool {
	return x % 2 == 0
})
println(even_fn)
words := ['hello', 'world']
upper := words.map(it.to_upper())
println(upper) // ['HELLO', 'WORLD']
// map también puede aceptar funciones anónimas
upper_fn := words.map(fn (w string) string {
	return w.to_upper()
})
println(upper_fn) // ['HELLO', 'WORLD']
```

`it` es una variable incorporada que se refiere al elemento que
se está procesando actualmente en los métodos filter/map.

#### Matriz multidimensional

Las matrices pueden tener más de una dimensión.

Ejemplo de matriz de 2 dimensiones:

```v
mut a := [][]int{len: 2, init: []int{len: 3}}
a[0][1] = 2
println(a) // [[0, 2, 0], [0, 0, 0]]
```

Ejemplo de matriz de 3 dimensiones:

```v
mut a := [][][]int{len: 2, init: [][]int{len: 3, init: []int{len: 2}}}
a[0][1][1] = 2
println(a) // [[[0, 0], [0, 2], [0, 0]], [[0, 0], [0, 0], [0, 0]]]
```

#### Ordenación de matrices

Ordenar matrices de todo tipo es muy sencillo e intuitivo. Las variables especiales `a` y `b`
se utilizan cuando se proporciona una condición de ordenación personalizada.

```v
mut numbers := [1, 3, 2]
numbers.sort() // 1, 2, 3
numbers.sort(a > b) // 3, 2, 1
```

```v nofmt
struct User {
	age  int
	name string
}

mut users := [User{21, 'Bob'}, User{20, 'Zarkon'}, User{25, 'Alice'}]
users.sort(a.age < b.age) // ordenación por el campo de entero User.age
users.sort(a.name > b.name) // ordenación inversa por el campo de cadena User.name
```

#### Cortes de una matriz (Array slices)

Los cortes son matrices parciales. Representan cada elemento entre dos índices
separados por un operador `..` El índice del lado derecho debe ser mayor o igual
al índice del lado izquierdo.

Si un índice del lado derecho está ausente, se asume que es la
longitud de la matriz. Si el índice del lado izquierdo está ausente, se asume que
es 0.

```v
nums := [1, 2, 3, 4, 5]
println(nums[1..4]) // [2, 3, 4]
println(nums[..4]) // [1, 2, 3, 4]
println(nums[1..]) // [2, 3, 4, 5]
```

Todas las operaciones de las matrices se pueden realizar sobre los cortes.
Los cortes pueden ser insertadas a una matriz del mismo tipo.

```v
array_1 := [3, 5, 4, 7, 6]
mut array_2 := [0, 1]
array_2 << array_1[..3]
println(array_2) // [0, 1, 3, 5, 4]
```

### Matrices de tamaño fijo

V también admite matrices de tamaño fijo. A diferencia de las matrices
ordinarias, su  longitud es constante. No se pueden añadir elementos a
ellas, ni reducirlas.
Sólo se pueden modificar sus elementos en su lugar.

Sin embargo, el acceso a los elementos de las matrices de tamaño fijo
es más eficiente, necesitan menos memoria que las matrices ordinarias
y, a diferencia de éstas, sus datos están en la pila, por lo que puedes
querer utilizarlos como buffers si no quieres asignaciones adicionales
de la pila.

La mayoría de los métodos están definidos para trabajar con matrices
ordinarias, no con matrices de tamaño fijo.
Se puede convertir un array de tamaño fijo en un array ordinario con
el rebanado:

```v
mut fnums := [3]int{} // fnums es una matriz de tamaño fijo con 3 elementos.
fnums[0] = 1
fnums[1] = 10
fnums[2] = 100
println(fnums) // => [1, 10, 100]
println(typeof(fnums).name) // => [3]int

anums := fnums[0..fnums.len]
println(anums) // => [1, 10, 100]
println(typeof(anums).name) // => []int
```

Ten en cuenta que el corte hará que los datos de la matriz de tamaño
fijo se copien en la nueva matriz ordinaria creada.

### Mapas

```v nofmt
mut m := map[string]int{} // un mapa con claves tipo `string` y valores tipo `int`.
m['one'] = 1
m['two'] = 2
println(m['one']) // "1"
println(m['bad_key']) // "0"
println('bad_key' in m) // utiliza `in` para detectar si dicha clave existe
println('bad_key_2' !in m) // utiliza `in` para detectar si dicha clave no existe
m.delete('two')
```

Los mapas pueden tener claves de tipo `cadenas`, `rune`, `enteros` o `voidptr`

Un mapa se puede inicializar usando esta breve sintaxis:

```v nofmt
numbers := map{
	1: 'one'
	2: 'two'
}
println(numbers)
```

Si no se encuentra una clave, se devuelve un valor cero por defecto:

```v nofmt
sm := map{
	'abc': 'xyz'
}
val := sm['bad_key']
println(val) // ''
intm := {
	1: 1234
	2: 5678
}
s := intm[3]
println(s) // 0
```

También es posible utilizar un bloque `or {}` para manejar las claves que faltan:

```v
mm := map[string]int{}
val := mm['bad_key'] or { panic('clave no encontrada') }
```

La misma comprobación opcional se aplica a las matrices:

```v
arr := [1, 2, 3]
large_index := 999
val := arr[large_index] or { panic('fuera de los límites') }
```

## Importación de módulos

Para obtener información sobre la creación de un módulo, consulte [Módulos](#modulos).

Los módulos pueden importarse mediante la palabra clave `import`:

```v
import os

fn main() {
	// leer texto desde stdin
	name := os.input('Ingresa tu nombre: ')
	println('¡Hola, $name!')
}
```

Este programa puede utilizar cualquier definición pública del módulo `os`,
como la función `input`. Consulte la documentación de la
[biblioteca estándar (en inglés)](https://modules.vlang.io/) para una lista de módulos
comunes y sus símbolos públicos.

Por defecto, hay que especificar el prefijo del módulo cada vez que se llama
a una función externa. Esto puede parecer verboso al principio, pero hace que
el código sea mucho más legible y más fácil de entender - siempre está claro
qué función de qué módulo se está llamando. Esto es especialmente útil en
bases de código grandes.

Las importaciones cíclicas de módulos no están permitidas, como en Go.

### Importación selectiva

También puedes importar directamente funciones y tipos específicos de los módulos

```v nofmt
import os { input }
import crypto.sha256 { sum }
import time { Time }
```

Nota: Esto no está permitido para las constantes - siempre deben llevar un prefijo.

Puedes importar varios símbolos específicos a la vez:

```v
import os { input, user_os }

name := input('Enter your name: ')
println('Name: $name')
os := user_os()
println('Your OS is ${os}.')
```

### Importando módulos con alias

Cualquier nombre de módulo importado puede ser aliasado utilizando la palabra clave `as`:

NOTA: este ejemplo no compilará a menos que hayas creado `mymod/sha256.v`.

```v failcompile
import crypto.sha256
import mymod.sha256 as mysha256

fn main() {
    v_hash := sha256.sum('hi'.bytes()).hex()
    my_hash := mysha256.sum('hi'.bytes()).hex()
    assert my_hash == v_hash
}
```

No se puede poner un alias a una función o tipo importado.
Sin embargo, _puede_ volver a declarar un tipo.


```v
import time
import math

type MyTime = time.Time

fn (mut t MyTime) century() int {
	return int(1.0 + math.trunc(f64(t.year) * 0.009999794661191))
}

fn main() {
	mut my_time := MyTime{
		year: 2020
		month: 12
		day: 25
	}
	println(time.new_time(my_time).utc_string())
	println('Century: $my_time.century()')
}
```
