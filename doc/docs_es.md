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

## Install from source

La forma principal de obtener la versión de V más reciente y mejorada es
__instalarlo desde la fuente__.
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
    * [Retornando múltiples valores](#retornando-multiples-valores)
    * [Numéro variable de argumentos](#numero-variable-de-argumentos)
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
[aquí](https://github.com/vlang/v/blob/master/README.md#symlinking).
Si aún no lo has hecho, debes escribir la ruta al ejecutable de V manualmente.

¡Felicitaciones, acaba de escribir y ejecutar su primer programa hecho en V!

También puedes compilar un programa sin ejecución con `v hello.v`.
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

// usando la descomposición de arrays
a := [2, 3, 4]
println(sum(...a)) // <-- usando el prefijo ... aquí. se imprime: 9
b := [5, 6, 7]
println(sum(...b)) // se imprime: 18
```
