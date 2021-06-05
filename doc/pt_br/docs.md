# V - Documentação

## Introdução

V é uma linguagem de programação compilada estaticamente tipada projetada
para a construção de software sustentável.


É semelhante a Go e seu design também foi influenciado por Oberon, Rust, Swift,
Kotlin e Python.

V é uma linguagem muito simples. Analisar esta documentação levará cerca de uma hora,
e no final você terá aprendido praticamente toda a linguagem.

A linguagem promove a escrita de código simples e claro com abstração mínima.

Apesar de ser simples, V dá ao desenvolvedor muito poder.
Tudo o que você pode fazer em outras linguagens, você pode fazer em V.

## Instalação a partir do código fonte
A principal forma de obter o melhor e mais recente V é __instalá-lo a partir do código-fonte__.
É __fácil__ e geralmente leva __apenas alguns segundos__.


### Linux, macOS, FreeBSD, etc:
Você precisa de `git` e um compilador C como` tcc`, `gcc` ou` clang` e `make`:
```bash
git clone https://github.com/vlang/v
cd v
make
```

### Windows:
Você precisa de `git` e um compilador C como` tcc`, `gcc`,` clang` ou `msvc`:
```bash
git clone https://github.com/vlang/v
cd v
make.bat -tcc
```
Obs: Você também pode passar um de `-gcc`,` -msvc`, `-clang` para` make.bat` em vez disso,
se você preferir usar um compilador C diferente, mas -tcc é pequeno, rápido e
fácil de instalar (V irá baixar um binário pré-construído automaticamente).

Recomenda-se adicionar esta pasta ao PATH de suas variáveis ​​de ambiente.
Isso pode ser feito com o comando `v.exe symlink`.

### Android
A execução de aplicativos gráficos V no Android
também é possível via [vab](https://github.com/vlang/vab).

Dependências do V no Android: **V**, **Java JDK** >= 8, Android **SDK + NDK**.

  1. Para instalar as dependências (veja [vab](https://github.com/vlang/vab))
  2. Conecte seu dispositivo Android
  3. Rode o comando:
  ```bash
  git clone https://github.com/vlang/vab && cd vab && v vab.v
  ./vab --device auto run /path/to/v/examples/sokol/particles
  ```
Para mais detalhes e solução de problemas,
visite o [repositório vab GitHub](https://github.com/vlang/vab).

## Índice

<table>
    <tr><td width=33% valign=top>

* [Hello world](#hello-world)
* [Executando uma pasta de projeto](#executando-uma-pasta-de-projeto-com-vários-arquivos)
* [Comentários](#comentários)
* [Funções](#funções)
    * [Retornando vários valores](#retornando-vários-valores)    
* [Visibilidade de Símbolo](#visibilidade-de-símbolo)
* [Variáveis](#variáveis)
* [Tipos](#tipos)
    * [Strings](#strings)
    * [Números](#números)    
    * [Arrays](#arrays)
    * [Arrays de tamanho fixo](#arrays-de-tamanho-fixo)    
    * [Maps](#maps)
* [Importação de módulos](#importação-de-módulos)
* [Declarações e expressões](#declarações-e-expressões)
    * [If](#if)
    * [Operador In](#operador-in)    
    * [For loop](#for-loop)
    * [Match](#match)
    * [Defer](#defer)
* [Structs](#structs)
    * [Embedded structs](#embedded-structs)
    * [Valores de campo padrão](#valores-de-campo-padrão)    
    * [Short struct literal syntax](#short-struct-initialization-syntax)
    * [Modificadores de Acesso](#modificadores-de-acesso)    
    * [Métodos](#métodos)    
* [Unions](#unions)

</td><td width=33% valign=top>

* [Funções 2](#funções-2)
    * [Funções puras por padrão](#funções-puras-por-padrão)    
    * [Argumentos mutáveis](#argumentos-mutáveis)
    * [Número variável de argumentos](#número-variável-de-argumentos)
    * [Funções anônimas e de alta ordem](#funções-anônimas-e-de-alta-ordem)
* [Referências](#referências)
* [Constantes](#constantes)
* [Funções Builtin](#funções-builtin)
* [Impressão de tipos personalizados](#impressão-de-tipos-personalizados)
* [Módulos](#módulos)
    * [Módulo Gerenciador de pacotes](#módulo-gerenciador-de-pacotes)
* [Tipos 2](#tipos-2)
    * [Interfaces](#interfaces)
    * [Enums](#enums)
    * [Sum types](#sum-types)
    * [Type aliases](#type-aliases)
    * [Option/Result types & error handling](#optionresult-types-and-error-handling)
* [Generics](#generics)
* [Concorrência](#concorrência)
    * [Spawning Concurrent Tasks](#spawning-concurrent-tasks)
    * [Channels](#channels)
    * [Objetos Compartilhados](#objetos-compartilhados)
* [Decoding JSON](#decoding-json)
* [Testing](#testing)
* [Gerenciamento de Memória](#gerenciamento-de-memória)
* [ORM](#orm)

</td><td valign=top>

* [Escrevendo a Documentação](#escrevendo-a-documentação)
* [Ferramentas](#ferramentas)
    * [v fmt](#v-fmt)
    * [Profiling](#profiling)
* [Tópicos Avançados](#tópicos-avançados)
    * [Dumping expressions at runtime](#dumping-expressions-at-runtime)
    * [Memory-unsafe code](#memory-unsafe-code)
    * [Structs com campos de referência](#structs-com-campos-de-referência)
    * [sizeof e __offsetof](#sizeof-and-__offsetof)
    * [Chamando C do V](#chamando-c-do-v)
    * [Debugando código C gerado](#debugando-código-c-gerado)
    * [Compilação Condicional](#compilação-condicional)
    * [Compile time pseudo variables](#compile-time-pseudo-variables)
    * [Compile-time reflection](#compile-time-reflection)
    * [Limited operator overloading](#limited-operator-overloading)
    * [Inline assembly](#inline-assembly)
    * [Traduzindo C para V](#traduzindo-c-para-v)
    * [Hot code reloading](#hot-code-reloading)
    * [Cross compilation](#cross-compilation)
    * [Cross-platform shell scripts em V](#cross-platform-shell-scripts-em-v)
    * [Attributes](#attributes)
    * [Goto](#goto)
* [Apêndices](#apêndices)
    * [Keywords](#appendix-i-keywords)
    * [Operators](#appendix-ii-operators)

</td></tr>
</table>

<!--
Obs: existem várias palavras-chave especiais,
que você pode colocar após as cercas de código para v:
compile, live, ignore, failcompile, oksyntax, badsyntax, wip, nofmt
Para mais detalhes, faça: `v check-md`
-->

## Hello World


```v
fn main() {
	println('hello world')
}
```

Salve este trecho em um arquivo chamado `hello.v`. Agora faça: `v execute hello.v`.

> Isso pressupõe que você vinculou simbolicamente seu V com `v symlink`, conforme descrito
[aqui](https://github.com/vlang/v/blob/master/README.md#symlinking).
Se ainda não o fez, você deve digitar o caminho para V manualmente.

Parabéns - você acabou de escrever e executar seu primeiro programa em V!

Você pode compilar um programa sem execução com `v hello.v`.
Veja `v help` para todos os comandos suportados.

No exemplo acima, você pode ver que as funções são declaradas com a palavra-chave `fn`.
O tipo de retorno é especificado após o nome da função.
Neste caso, `main` não retorna nada, então não há tipo de retorno.

Como em muitas outras linguagens (como C, Go e Rust), `main`
é o ponto de entrada do seu programa.

`println` é uma das poucas funções embutidas.
Ele imprime o valor passado para a saída padrão.

A declaração `fn main ()` pode ser ignorada em um arquivo de programas.
Isso é útil ao escrever pequenos programas, "scripts" ou apenas aprender a linguagem.
Para resumir, `fn main ()` será ignorado neste tutorial.

Isso significa que um programa "hello world" em V é tão simples quanto

```v
println('hello world')
```

## Executando uma pasta de projeto com vários arquivos

Suponha que você tenha uma pasta com vários arquivos .v, onde um deles
contém sua função `main ()`, e os outros arquivos têm outras
funções auxiliares. Eles podem ser organizados por tópicos, mas ainda * não * estruturados
o suficiente para serem seus próprios módulos reutilizáveis ​​separados,
e você deseja compilar
todos eles em um programa.

Em outras palavras, você teria que usar includes ou um sistema de compilação
para enumerar todos os arquivos, compilá-los separadamente para arquivos-objeto,
em seguida, vinculá-los a um executável final.

No V, no entanto, você pode compilar e executar toda a pasta de arquivos .v juntos,
usando apenas `v run .`. Passar parâmetros também funciona, então você pode
fazer: `v run. --seuparamalgumas_outras_coisas`

O comando acima irá primeiro compilar seus arquivos em um único programa (chamado
após sua pasta / projeto), e então executará o programa com
`--seuparamalgumas_outras_coisas` passado para ele como parâmetros CLI.

Seu programa pode então usar os parâmetros CLI como este:
```v
import os

println(os.args)
```
Obs: após uma execução bem-sucedida, V irá deletar o executável gerado.
Se você quiser mantê-lo, use `v -keepc run .` em vez disso, ou apenas compile
manualmente com `v .`.

Obs: qualquer sinalizador do compilador V deve ser passado * antes * do comando `run`.
Tudo após o arquivo / pasta de origem, será passado para o programa
como está - não será processado por V.

## Comentários

```v
// Esta é uma única linha de comentário
/*
Este é um comentário multiline
   /* Pode ser aninhado. */
*/
```

## Funções

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

Novamente, o tipo vem depois do nome do argumento.

Assim como em Go e C, as funções não podem ser sobrecarregadas.
Isso simplifica o código e melhora a capacidade de manutenção e leitura.

As funções podem ser usadas antes de sua declaração:
`add` e` sub` são declarados após `main`, mas ainda podem ser chamados de` main`.
Isso é verdade para todas as declarações em V e elimina a necessidade de arquivos de cabeçalho
ou pensando na ordem dos arquivos e declarações.

### Retornando vários valores

```v
fn foo() (int, int) {
	return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
c, _ := foo() // ignore os valores usando `_`
```

## Visibilidade de Símbolo

```v
pub fn public_function() {
}

fn private_function() {
}
```

As funções são privadas (não exportadas) por padrão.
Para permitir que outros módulos as usem, acrescente `pub`. O mesmo se aplica
para constantes e tipos.

Nota: `pub` só pode ser usado a partir de um módulo nomeado.
Para obter informações sobre como criar um módulo, consulte [Módulos] (#módulos).

## Variáveis

```v
name := 'Bob'
age := 20
large_number := i64(9999999999)
println(name)
println(age)
println(large_number)
```

As variáveis ​​são declaradas e inicializadas com `:=`. Este é o único
maneira de declarar variáveis ​​em V. Isso significa que as variáveis ​​sempre têm um
valor.

O tipo da variável é inferido do valor do lado direito.
Para escolher um tipo diferente, use a conversão de tipo:
a expressão `T (v)` converte o valor `v` para o tipo `T`.

Ao contrário da maioria das outras linguagens, V só permite definir variáveis ​​em funções.
Variáveis ​​globais (nível de módulo) não são permitidas. Não há estado global em V
(consulte [Funções puras por padrão](#funções-puras-por-padrão) para obter detalhes).

Para consistência em diferentes bases de código, todos os nomes de variáveis ​​e funções
devem usar o estilo `snake_case`, ao contrário dos nomes de tipo, que devem usar` PascalCase`.

### Variáveis Mutáveis

```v
mut age := 20
println(age)
age = 21
println(age)
```

Para alterar o valor da variável, use `=`. Em V, as variáveis ​​são
imutáveis por padrão.
Para poder alterar o valor da variável, você deve declará-la com `mut`.

Tente compilar o programa acima após remover `mut` da primeira linha.

### Inicialização vs atribuição

Observe a (importante) diferença entre `: =` e `=`.
`: =` é usado para declarar e inicializar, `=` é usado para atribuir.

```v failcompile
fn main() {
	age = 21
}
```

Este código não irá compilar, porque a variável `age` não está declarada.
Todas as variáveis ​​precisam ser declaradas em V.

```v
fn main() {
	age := 21
}
```

Os valores de várias variáveis ​​podem ser alterados em uma linha.
Desta forma, seus valores podem ser trocados sem uma variável intermediária.

```v
mut a := 0
mut b := 1
println('$a, $b') // 0, 1
a, b = b, a
println('$a, $b') // 1, 0
```

### Erros de declaração

No modo de desenvolvimento, o compilador irá avisá-lo de que você não usou a variável
(você receberá um aviso de "variável não utilizada").
No modo de produção (habilitado passando a sinalização `-prod` para v -` v -prod foo.v`)
ele não irá compilar (como em Go).

```v failcompile nofmt
fn main() {
	a := 10
	if true {
		a := 20 // error: redefinition of `a`
	}
	// warning: unused variable `a`
}
```

Ao contrário da maioria das linguagens, o sombreamento variável não é permitido.
Declarando uma variável com um nome
que já é usado em um escopo pai causará um erro de compilação.

No entanto, você pode criar sombra nos módulos importados,
pois isso é muito útil em algumas situações:
```v ignore
import ui
import gg

fn draw(ctx &gg.Context) {
	gg := ctx.parent.get_ui().gg
	gg.draw_rect(10, 10, 100, 50)
}
```

## Tipos

### Tipos Primitivos

```v ignore
bool

string

i8    i16  int  i64      i128 (soon)
byte  u16  u32  u64      u128 (soon)

rune // representa um ponto de código Unicode

f32 f64

voidptr, size_t // estes são usados ​​principalmente para interoperabilidade C

any // semelhante ao void * de C e à interface de Go {}
```

Observe que, ao contrário de C e Go, `int` é sempre um número inteiro de 32 bits.

Há uma exceção à regra de que todos os operadores
em V deve ter valores do mesmo tipo em ambos os lados. Um pequeno tipo primitivo
de um lado pode ser promovido automaticamente se se encaixar
completamente no intervalo de dados do tipo do outro lado.
Estas são as possibilidades permitidas:

```v ignore
   i8 → i16 → int → i64
                  ↘     ↘
                    f32 → f64
                  ↗     ↗
 byte → u16 → u32 → u64 ⬎
      ↘     ↘     ↘      ptr
   i8 → i16 → int → i64 ⬏
```
Um valor `int`, por exemplo, pode ser promovido automaticamente para` f64`
ou `i64`, mas não para` u32`. (`u32` significaria perda do sinal para
valores negativos).
A promoção de `int` para` f32`, no entanto, atualmente é feita automaticamente
(mas pode levar à perda de precisão para valores grandes).

Literais como `123` ou` 4.56` são tratados de uma maneira especial. Eles
não levam a promoções de tipo, no entanto, o padrão é `int` e` f64`
respectivamente, quando seu tipo deve ser decidido:

```v nofmt
u := u16(12)
v := 13 + u    // v is of type `u16` - no promotion
x := f32(45.6)
y := x + 3.14  // x is of type `f32` - no promotion
a := 75        // a is of type `int` - default for int literal
b := 14.7      // b is of type `f64` - default for float literal
c := u + a     // c is of type `int` - automatic promotion of `u`'s value
d := b + x     // d is of type `f64` - automatic promotion of `x`'s value
```

### Strings

```v
name := 'Bob'
println(name.len)
println(name[0]) // indexing gives a byte B
println(name[1..3]) // slicing gives a string 'ob'
windows_newline := '\r\n' // escape special characters like in C
assert windows_newline.len == 2
```

Em V, uma string é uma matriz de bytes somente leitura.
Os dados da string são codificados usando UTF-8.
Os valores da string são imutáveis. Você não pode modificar elementos:

```v failcompile
mut s := 'hello 🌎'
s[0] = `H` // not allowed
```
> error: cannot assign to `s[i]` since V strings are immutable

Observe que indexar uma string produzirá um `byte`, não um` rune`.
Índices correspondem para bytes na string, não pontos de código Unicode.

Literais de caracteres são do tipo `rune`. Para denotá-los, use `

```v
rocket := `🚀`
assert 'aloha!'[0] == `a`
```

As aspas simples e duplas podem ser usadas para denotar strings.
Para consistência, `vfmt` converte aspas duplas em aspas simples,
a menos que a string contenha um caractere de aspas simples.

Para strings brutas, acrescente `r`. Strings brutos não são escapados:

```v
s := r'hello\nworld'
println(s) // "hello\nworld"
```

Strings podem ser facilmente convertidos em inteiros:

```v
s := '42'
n := s.int() // 42
```

### Interpolação de String

A sintaxe de interpolação básica é muito simples - use `$`
antes do nome de uma variável.
A variável será convertida em uma string e incorporada ao literal:
```v
name := 'Bob'
println('Hello, $name!') // Hello, Bob!
```
Também funciona com os campos: `'age = $ user.age'`.
Se você precisar de expressões mais complexas, use `$ {}`: `'can register = $ {user.age> 13}'`.

Especificadores de formato semelhantes àqueles em `printf ()` do C também são suportados.
`f`,` g`, `x`, etc. são opcionais e especificam o formato de saída.
O compilador cuida do tamanho do armazenamento, para que não haja `hd` ou` llu`.

```v
x := 123.4567
println('x = ${x:4.2f}')
println('[${x:10}]') // pad with spaces on the left => [   123.457]
println('[${int(x):-10}]') // pad with spaces on the right => [123       ]
println('[${int(x):010}]') // pad with zeros on the left => [0000000123]
```

### String operators

```v
name := 'Bob'
bobby := name + 'by' // + is used to concatenate strings
println(bobby) // "Bobby"
mut s := 'hello '
s += 'world' // `+=` is used to append to a string
println(s) // "hello world"
```

Todos os operadores em V devem ter valores do mesmo tipo em ambos os lados.
Você não pode concatenar um inteiro com uma string:

```v failcompile
age := 10
println('age = ' + age) // not allowed
```
> error: infix expr: cannot use `int` (right expression) as `string`

Temos que converter `idade` em uma` string`:

```v
age := 11
println('age = ' + age.str())
```

ou use interpolação de string (preferencial):

```v
age := 12
println('age = $age')
```

### Números

```v
a := 123
```

Isso atribuirá o valor de 123 a `a`. Por padrão, `a` terá o
digite `int`.

Você também pode usar notação hexadecimal, binária ou octal para literais inteiros:

```v
a := 0x7B
b := 0b01111011
c := 0o173
```

Todos eles serão atribuídos ao mesmo valor, 123. Todos eles terão tipo
`int`, não importa a notação que você usou.

V também suporta a escrita de números com `_` como separador:

```v
num := 1_000_000 // same as 1000000
three := 0b0_11 // same as 0b11
float_num := 3_122.55 // same as 3122.55
hexa := 0xF_F // same as 255
oct := 0o17_3 // same as 0o173
```

Se quiser um tipo diferente de número inteiro, você pode usar a conversão:

```v
a := i64(123)
b := byte(42)
c := i16(12345)
```

A atribuição de números de ponto flutuante funciona da mesma maneira:

```v
f := 1.0
f1 := f64(3.14)
f2 := f32(3.14)
```
Se você não especificar o tipo explicitamente, por padrão, literais flutuantes
terá o tipo `f64`.

### Arrays

```v
mut nums := [1, 2, 3]
println(nums) // "[1, 2, 3]"
println(nums[1]) // "2"
nums[1] = 5
println(nums) // "[1, 5, 3]"
println(nums.len) // "3"
nums = [] // The array is now empty
println(nums.len) // "0"
// Declare an empty array:
users := []int{}
```

O tipo de uma matriz é determinado pelo primeiro elemento:
* `[1, 2, 3]` é uma matriz de ints (`[] int`).
* `['a', 'b']` é um array de strings (`[] string`).

O usuário pode especificar explicitamente o tipo para o primeiro elemento:
`[byte (16), 32, 64, 128]`.
As matrizes V são homogêneas (todos os elementos devem ter o mesmo tipo).
Isso significa que código como `[1, 'a']` não será compilado.

O campo `.len` retorna o comprimento da matriz.
Observe que é um campo somente leitura,
e não pode ser modificado pelo usuário.
Os campos exportados são somente leitura por padrão em V.
Veja [modificadores de acesso] (#modificadores-de-acesso).

#### Array operations

```v
mut nums := [1, 2, 3]
nums << 4
println(nums) // "[1, 2, 3, 4]"
// append array
nums << [5, 6, 7]
println(nums) // "[1, 2, 3, 4, 5, 6, 7]"
mut names := ['John']
names << 'Peter'
names << 'Sam'
// names << 10  <-- This will not compile. `names` is an array of strings.
println(names.len) // "3"
println('Alex' in names) // "false"
```

`<<`  é um operador que anexa um valor ao final da matriz.
Ele também pode anexar uma matriz inteira.

`val in array` retorna verdadeiro se o array contém` val`.
Veja [operador `in`](#operador-in).

#### Inicializando propriedades de array

Durante a inicialização, você pode especificar a capacidade da matriz (`cap`),
seu comprimento inicial (` len`),
e o elemento padrão (`init`):

```v
arr := []int{len: 5, init: -1}
// `[-1, -1, -1, -1, -1]`
```

Definir a capacidade melhora o desempenho das inserções,
pois reduz o número de realocações necessárias:

```v
mut numbers := []int{cap: 1000}
println(numbers.len) // 0
// Now appending elements won't reallocate
for i in 0 .. 1000 {
	numbers << i
}
```
Nota: O código acima usa uma instrução [range `for`](#range-for).

#### Métodos do Array

Todos os arrays podem ser facilmente impressos com `println (arr)`
e convertidos em uma string
com `s: = arr.str ()`.

Copiar os dados do array é feito com `.clone ()`:

```v
nums := [1, 2, 3]
nums_copy := nums.clone()
```

Arrays podem ser filtrados e mapeados de forma eficiente com `.filter ()` e
métodos `.map ()`:

```v
nums := [1, 2, 3, 4, 5, 6]
even := nums.filter(it % 2 == 0)
println(even) // [2, 4, 6]
// filter pode aceitar funções anônimas
even_fn := nums.filter(fn (x int) bool {
	return x % 2 == 0
})
println(even_fn)
words := ['hello', 'world']
upper := words.map(it.to_upper())
println(upper) // ['HELLO', 'WORLD']
// map também pode aceitar funções anônimas
upper_fn := words.map(fn (w string) string {
	return w.to_upper()
})
println(upper_fn) // ['HELLO', 'WORLD']
```

`it` é uma variável embutida que se refere ao elemento
atualmente sendo processado nos métodos de filtro/mapa.

Além disso, `.any ()` e `.all ()` podem ser usados ​​para testar convenientemente
para elementos que satisfaçam uma condição.

```v
nums := [1, 2, 3]
println(nums.any(it == 2)) // true
println(nums.all(it >= 2)) // false
```

#### Arrays Multidimensionais

Arrays podem ter mais de uma dimensão.

Exemplo 2d array:
```v
mut a := [][]int{len: 2, init: []int{len: 3}}
a[0][1] = 2
println(a) // [[0, 2, 0], [0, 0, 0]]
```

Exemplo 3d array:
```v
mut a := [][][]int{len: 2, init: [][]int{len: 3, init: []int{len: 2}}}
a[0][1][1] = 2
println(a) // [[[0, 0], [0, 2], [0, 0]], [[0, 0], [0, 0], [0, 0]]]
```

#### Sorting arrays

Sorting arrays de todos os tipos são muito simples e intuitivos.
Variáveis ​​especiais `a` e` b`
são usados ​​ao fornecer uma condição de classificação personalizada.

```v
mut numbers := [1, 3, 2]
numbers.sort() // 1, 2, 3
numbers.sort(a > b) // 3, 2, 1
```

```v
struct User {
	age  int
	name string
}

mut users := [User{21, 'Bob'}, User{20, 'Zarkon'}, User{25, 'Alice'}]
users.sort(a.age < b.age) // sort by User.age int field
users.sort(a.name > b.name) // reverse sort by User.name string field
```

#### Array Slices

Slices são arrays parciais. Eles representam cada elemento entre dois índices
separados por um operador ... O índice do lado direito deve ser maior ou igual
para o índice do lado esquerdo.

Se um índice do lado direito estiver ausente, será considerado o comprimento da matriz.
Se um índice do lado esquerdo está ausente, é assumido como 0.

```v
nums := [0, 10, 20, 30, 40]
println(nums[1..4]) // [10, 20, 30]
println(nums[..4]) // [0, 10, 20, 30]
println(nums[1..]) // [10, 20, 30, 40]
```

Todas as operações de array podem ser realizadas em fatias.
As fatias podem ser colocadas em uma matriz do mesmo tipo.

```v
array_1 := [3, 5, 4, 7, 6]
mut array_2 := [0, 1]
array_2 << array_1[..3]
println(array_2) // [0, 1, 3, 5, 4]
```

### Arrays de tamanho fixo

V também oferece suporte a arrays com tamanho fixo. Ao contrário de arrays comuns,
seus o comprimento é constante. Você não pode anexar elementos a eles, nem reduzi-los.
Você só pode modificar seus elementos no local.

No entanto, o acesso aos elementos de arrays de tamanho fixo é mais eficiente,
eles precisam de menos memória do que arrays comuns e, ao contrário de matrizes comuns,
seus dados estão na pilha, então você pode querer usá-los como buffers se você
não deseja alocações de heap adicionais.

A maioria dos métodos é definida para funcionar em arrays comuns, não em arrays de tamanho fixo.
Você pode converter um array de tamanho fixo em um array comum com fatiamento:
```v
mut fnums := [3]int{} // fnums é um array de tamanho fixo com 03 elementos.
fnums[0] = 1
fnums[1] = 10
fnums[2] = 100
println(fnums) // => [1, 10, 100]
println(typeof(fnums).name) // => [3]int

fnums2 := [1, 10, 100]! // short init syntax that does the same (the syntax will probably change)

anums := fnums[0..fnums.len]
println(anums) // => [1, 10, 100]
println(typeof(anums).name) // => []int
```
Observe que o fatiamento fará com que os dados da matriz de tamanho fixo sejam copiados para
o array comum recém-criado.

### Maps

```v
mut m := map[string]int{} // um map com chaves `string` e valores `int`
m['one'] = 1
m['two'] = 2
println(m['one']) // "1"
println(m['bad_key']) // "0"
println('bad_key' in m) // Use `in` para detectar se essa chave existe
m.delete('two')
```
Os maps podem ter chaves do tipo string, rune, integer, float ou voidptr.

Todo o mapa pode ser inicializado usando esta sintaxe curta:
```v
numbers := map{
	'one': 1
	'two': 2
}
println(numbers)
```

Se uma chave não for encontrada, um valor zero é retornado por padrão:

```v
sm := map{
	'abc': 'xyz'
}
val := sm['bad_key']
println(val) // ''
```
```v
intm := map{
	1: 1234
	2: 5678
}
s := intm[3]
println(s) // 0
```

Também é possível usar um bloco `or{}` para lidar com as chaves ausentes:

```v
mm := map[string]int{}
val := mm['bad_key'] or { panic('key not found') }
```

A mesma verificação opcional se aplica a arrays:

```v
arr := [1, 2, 3]
large_index := 999
val := arr[large_index] or { panic('out of bounds') }
```

## Importação de Módulos

Para obter informações sobre como criar um módulo, consulte [Módulos](#módulos).

Os módulos podem ser importados usando a palavra-chave `import`:

```v
import os

fn main() {
	// lê o texto a partir do stdin
	name := os.input('Enter your name: ')
	println('Hello, $name!')
}
```
Este programa pode usar qualquer definição pública do módulo `os`, como
a função `input`. Veja a documentação da [biblioteca padrão](https://modules.vlang.io/)
para uma lista de módulos comuns e seus símbolos públicos.

Por padrão, você deve especificar o prefixo do módulo sempre que chamar uma função externa.
Isso pode parecer prolixo no início, mas torna o código muito mais legível
e mais fácil de entender - é sempre claro qual função de
qual módulo está sendo chamado. Isso é especialmente útil em grandes bases de código.

Importações de módulos cíclicos não são permitidas, como em Go.

### Importações seletivas

Você também pode importar funções e tipos específicos de módulos diretamente:

```v
import os { input }

fn main() {
	// lê o texto a partir do stdin
	name := input('Enter your name: ')
	println('Hello, $name!')
}
```
Nota: Isso não é permitido para constantes - elas devem ser sempre prefixadas.

Você pode importar vários símbolos específicos de uma vez:

```v
import os { input, user_os }

name := input('Enter your name: ')
println('Name: $name')
os := user_os()
println('Your OS is ${os}.')
```

### Aliasing de importação de módulo

Qualquer nome de módulo importado pode receber um alias usando a palavra-chave `as`:

NOTA: este exemplo não irá compilar a menos que você tenha criado `mymod/sha256.v`
```v failcompile
import crypto.sha256
import mymod.sha256 as mysha256

fn main() {
	v_hash := mysha256.sum('hi'.bytes()).hex()
	my_hash := mysha256.sum('hi'.bytes()).hex()
	assert my_hash == v_hash
}
```

Você não pode criar um alias para uma função ou tipo importado.
No entanto, você _pode_ redeclarar um tipo.

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

## Declarações e expressões

### If

```v
a := 10
b := 20
if a < b {
	println('$a < $b')
} else if a > b {
	println('$a > $b')
} else {
	println('$a == $b')
}
```

As instruções `if` são bastante diretas e semelhantes à maioria das outras linguagens.
Ao contrário de outras linguagens semelhantes a C,
não há parênteses ao redor da condição e as chaves são sempre necessárias.

`if` pode ser usado como uma expressão:

```v
num := 777
s := if num % 2 == 0 { 'even' } else { 'odd' }
println(s)
// "odd"
```

#### Verificações de tipo e conversão
Você pode verificar o tipo atual de um tipo de soma usando `is` e sua forma negada`!Is`.

Você pode fazer isso em um `if`:
```v
struct Abc {
	val string
}

struct Xyz {
	foo string
}

type Alphabet = Abc | Xyz

x := Alphabet(Abc{'test'}) // sum type
if x is Abc {
	// x é automaticamente "casteado" para Abc e pode ser usado aqui
	println(x)
}
if x !is Abc {
	println('Not Abc')
}
```
ou usando `match`:
```v oksyntax
match x {
	Abc {
		// x é automaticamente "casteado" para Abc e pode ser usado aqui
		println(x)
	}
	Xyz {
		// x é automaticamente "casteado" para Xyz e pode ser usado aqui
		println(x)
	}
}
```

Isso também funciona com campos de struct
```v
struct MyStruct {
	x int
}

struct MyStruct2 {
	y string
}

type MySumType = MyStruct | MyStruct2

struct Abc {
	bar MySumType
}

x := Abc{
	bar: MyStruct{123} // MyStruct será convertido para MySumType automaticamente
}
if x.bar is MyStruct {
	// x.bar é automaticamente "casteado"
	println(x.bar)
}
match x.bar {
	MyStruct {
		// x.bar é automaticamente "casteado"
		println(x.bar)
	}
	else {}
}
```

Variáveis ​​mutáveis ​​podem mudar. E fazer uma conversão não seria seguro.
No entanto, às vezes é útil "castear" o tipo apesar da mutabilidade.
Nesses casos, o desenvolvedor deve marcar a expressão com a palavra-chave `mut`
para dizer ao compilador que eles sabem o que estão fazendo.

Funciona assim:
```v oksyntax
mut x := MySumType(MyStruct{123})
if mut x is MyStruct {
	// x is casted to MyStruct even if it's mutable
	// without the mut keyword that wouldn't work
	println(x)
}
// same with match
match mut x {
	MyStruct {
		// x is casted to MyStruct even it's mutable
		// without the mut keyword that wouldn't work
		println(x)
	}
}
```

### Operador In

`in` permite verificar se um array ou map contém um elemento.
Para fazer o oposto, use `!In`.

```v
nums := [1, 2, 3]
println(1 in nums) // true
println(4 !in nums) // true
m := map{
	'one': 1
	'two': 2
}
println('one' in m) // true
println('three' !in m) // true
```

Também é útil para escrever expressões booleanas mais claras e compactas:

```v
enum Token {
	plus
	minus
	div
	mult
}

struct Parser {
	token Token
}

parser := Parser{}
if parser.token == .plus || parser.token == .minus || parser.token == .div || parser.token == .mult {
	// ...
}
if parser.token in [.plus, .minus, .div, .mult] {
	// ...
}
```

V otimiza tais expressões,
então ambas as instruções `if` acima
produzem o mesmo código de máquina e nenhum array é criado.

### For loop

V tem apenas uma palavra-chave de loop: `for`, com várias formas.

#### `for`/`in`

Esta é a forma mais comum. Você pode usá-lo com um array, map ou
intervalo numérico.

##### Array `for`

```v
numbers := [1, 2, 3, 4, 5]
for num in numbers {
	println(num)
}
names := ['Sam', 'Peter']
for i, name in names {
	println('$i) $name')
	// Output: 0) Sam
	//         1) Peter
}
```

A forma `for value in arr` é usada para percorrer os elementos de um array.
Se um índice for necessário, uma forma alternativa `for index, value in arr` pode ser usada.

Observe que o valor é somente leitura.
Se precisar modificar o array durante o loop, você precisará declarar o elemento como mutável:

```v
mut numbers := [0, 1, 2]
for mut num in numbers {
	num++
}
println(numbers) // [1, 2, 3]
```
Quando um identificador é apenas um único underscore, ele é ignorado.

##### Map `for`

```v
m := map{
	'one': 1
	'two': 2
}
for key, value in m {
	println('$key -> $value')
	// Output: one -> 1
	//         two -> 2
}
```

A chave ou o valor podem ser ignorados usando um único underscore como identificador.
```v
m := map{
	'one': 1
	'two': 2
}
// itera sobre as chaves
for key, _ in m {
	println(key)
	// Output: one
	//         two
}
// itera sobre os valores
for _, value in m {
	println(value)
	// Output: 1
	//         2
}
```

##### Range `for`

```v
// Prints '01234'
for i in 0 .. 5 {
	print(i)
}
```
`low..high` significa um intervalo * exclusivo *, que representa todos os valores
de `low` até * mas não incluindo *`high`.

#### Condition `for`

```v
mut sum := 0
mut i := 0
for i <= 100 {
	sum += i
	i++
}
println(sum) // "5050"
```

Esta forma de loop é semelhante aos loops `while` em outras linguagens.
O loop irá parar de iterar assim que a condição booleana for avaliada como falsa.
Novamente, não há parênteses em torno da condição e as chaves são sempre necessárias.

#### Bare `for`

```v
mut num := 0
for {
	num += 2
	if num >= 10 {
		break
	}
}
println(num) // "10"
```

A condição pode ser omitida, resultando em um loop infinito.

#### C `for`

```v
for i := 0; i < 10; i += 2 {
	// Don't print 6
	if i == 6 {
		continue
	}
	println(i)
}
```

Por fim, existe o tradicional loop `for` do estilo C. É mais seguro do que a forma `while`
porque com o último é fácil esquecer de atualizar o contador e ficar
preso em um loop infinito.

Aqui, `i` não precisa ser declarado com `mut`, pois sempre será mutável por definição.

#### Labelled break & continue

`break` e` continue` controlam o loop `for` mais interno por padrão.
Você também pode usar `break` e` continue`
seguido por um nome de rótulo para se referir a um `for` externo
ciclo:

```v
outer: for i := 4; true; i++ {
	println(i)
	for {
		if i < 7 {
			continue outer
		} else {
			break outer
		}
	}
}
```
O rótulo deve preceder imediatamente o loop externo.
O código acima é impresso:
```
4
5
6
7
```

### Match

```v
os := 'windows'
print('V is running on ')
match os {
	'darwin' { println('macOS.') }
	'linux' { println('Linux.') }
	else { println(os) }
}
```

Uma declaração de correspondência é uma maneira mais curta
de escrever uma sequência de declarações `if - else`.
Quando uma ramificação correspondente for encontrada,
o seguinte bloco de instrução será executado.
O outro branch será executado quando nenhum outro branch corresponder.

```v
number := 2
s := match number {
	1 { 'one' }
	2 { 'two' }
	else { 'many' }
}
```

Uma expressão de correspondência retorna o valor
da expressão final da ramificação correspondente.

```v
enum Color {
	red
	blue
	green
}

fn is_red_or_blue(c Color) bool {
	return match c {
		.red, .blue { true } // vírgula pode ser usada para testar vários valores
		.green { false }
	}
}
```

Uma declaração de correspondência também pode ser usada
para ramificar nas variantes de um `enum`
usando a sintaxe abreviada `.variant_here`.
Um branch `else` não é permitido
quando todos os ramos são exaustivos.

```v
c := `v`
typ := match c {
	`0`...`9` { 'digit' }
	`A`...`Z` { 'uppercase' }
	`a`...`z` { 'lowercase' }
	else { 'other' }
}
println(typ)
// 'lowercase'
```

Você também pode usar intervalos como padrões `match`. Se o valor estiver dentro do intervalo
de uma ramificação, essa ramificação será executada.

Observe que os intervalos usam `...` (três pontos) em vez de `..` (dois pontos). Isso é
porque o intervalo é * inclusivo * do último elemento, em vez de exclusivo
(como os intervalos `..` são). Usar `..` em um branch de correspondência gerará um erro.

Nota: `match` como uma expressão não é utilizável em loop` for` e declarações `if`.

### Defer

Uma declaração defer adia a execução de um bloco de declarações
até que a função circundante retorne.

```v
import os

fn read_log() {
	mut ok := false
	mut f := os.open('log.txt') or { panic(err.msg) }
	defer {
		f.close()
	}
	// ...
	if !ok {
		// defer statement será chamado aqui e o arquivo será fechado
		return
	}
	// ...
	// defer statement será chamado aqui e o arquivo será fechado
}
```

Se a função retornar um valor, o bloco `defer` é executado * após * o retorno
expressão é avaliada:

```v
import os

enum State {
	normal
	write_log
	return_error
}

// escreve o arquivo de log e retorna o número de bytes escrito
fn write_log(s State) ?int {
	mut f := os.create('log.txt') ?
	defer {
		f.close()
	}
	if s == .write_log {
		// `f.close()` será chamado após `f.write()` ser executado,
		// mas antes `write_log()` finalmente retorna o número de bytes
		// escrito para o `main()`
		return f.writeln('This is a log file')
	} else if s == .return_error {
		// o arquivo será fechado após a função `error()`
		// ter retornado - portanto, a mensagem de erro ainda o relatará como aberto
		return error('nothing written; file open: $f.is_opened')
	}
	// o arquivo também será fechado aqui
	return 0
}

fn main() {
	n := write_log(.return_error) or {
		println('Error: $err')
		0
	}
	println('$n bytes written')
}
```

## Structs

```v
struct Point {
	x int
	y int
}

mut p := Point{
	x: 10
	y: 20
}
println(p.x) // Struct fields are accessed using a dot
// Sintaxy literal alternativa para struct com 03 campos ou menos
p = Point{10, 20}
assert p.x == 10
```

### Heap structs

Structs são alocados na stack. Para alocar uma estrutura no heap
e obter uma referência a ele, use o prefixo `&`:

```v
struct Point {
	x int
	y int
}

p := &Point{10, 10}
// Referências tem a mesma sintaxe para acessar os campos
println(p.x)
```

O tipo de `p` é` & Point`. É uma [referência](#referências) para `Point`.
As referências são semelhantes aos ponteiros Go e referências C++.

### Embedded structs

V não permite subclasses, mas suporta estruturas incorporadas:

```v
struct Widget {
mut:
	x int
	y int
}

struct Button {
	Widget
	title string
}

mut button := Button{
	title: 'Click me'
}
button.x = 3
```
Sem incorporar, teríamos que nomear o campo `Widget` e fazer:

```v oksyntax
button.widget.x = 3
```

### Default field values

```v
struct Foo {
	n   int    // n é 0 por padrão
	s   string // s é '' por padrão
	a   []int  // a é `[]int{}` por padrão
	pos int = -1 // valor padrão customizado
}
```

Todos os campos de structs são zerados por padrão durante a criação da struct.
Campos de array e map são alocados.

Também é possível definir valores padrão personalizados.

### Required fields

```v
struct Foo {
	n int [required]
}
```

Você pode marcar um campo de estrutura com o atributo `[required]`, para dizer ao V que
esse campo deve ser inicializado ao criar uma instância dessa estrutura.

Este exemplo não irá compilar, uma vez que o campo `n` não foi inicializado explicitamente:
```v failcompile
_ = Foo{}
```

<a id='short-struct-initialization-syntax' />

### Short struct literal syntax

```v
struct Point {
	x int
	y int
}

mut p := Point{
	x: 10
	y: 20
}
// você pode omitir o nome do struct quando já é conhecido
p = {
	x: 30
	y: 4
}
assert p.y == 4
```

Omitir o nome da estrutura também funciona para retornar uma estrutura literal ou passar uma
como um argumento de função.

#### Trailing struct literal arguments

V não tem argumentos de função padrão ou argumentos nomeados, para essa estrutura final
a sintaxe literal pode ser usada em seu lugar:

```v
struct ButtonConfig {
	text        string
	is_disabled bool
	width       int = 70
	height      int = 20
}

struct Button {
	text   string
	width  int
	height int
}

fn new_button(c ButtonConfig) &Button {
	return &Button{
		width: c.width
		height: c.height
		text: c.text
	}
}

button := new_button(text: 'Click me', width: 100)
// o height não está definido, então é o valor padrão
assert button.height == 20
```

Como você pode ver, o nome da estrutura e as chaves podem ser omitidos, em vez de:

```v oksyntax nofmt
new_button(ButtonConfig{text:'Click me', width:100})
```

Isso só funciona para funções que usam uma estrutura para o último argumento.

### Access modifiers

Os campos Struct são privados
e imutáveis ​​por padrão (tornando os structs imutáveis ​​também).
Seus modificadores de acesso podem ser alterados com
`pub` e` mut`. No total, existem 05 opções possíveis:

```v
struct Foo {
	a int // private immutable (default)
mut:
	b int // private mutable
	c int // (você pode listar vários campos com o mesmo modificador de acesso)
pub:
	d int // public immutable (readonly)
pub mut:
	e int // public, mas mutável somente no módulo pai
__global:
	// (não é recomendado usar. Por isso, a palavra-chave 'global' começa com __)
	f int // public e mutável ambos dentro e fora do módulo pai
}
```

Por exemplo, aqui está o tipo `string` definido no módulo `builtin`:

```v ignore
struct string {
    str &byte
pub:
    len int
}
```

É fácil ver a partir desta definição que `string` é um tipo imutável.
O ponteiro de byte com os dados da string não pode ser acessado
fora de `builtin` de forma alguma.
O campo `len` é público, mas imutável:
```v failcompile
fn main() {
	str := 'hello'
	len := str.len // OK
	str.len++ // Compilation error
}
```

Isso significa que definir campos públicos somente leitura é muito fácil em V,
não há necessidade de getters / setters ou propriedades.

## Métodos

```v
struct User {
	age int
}

fn (u User) can_register() bool {
	return u.age > 16
}

user := User{
	age: 10
}
println(user.can_register()) // "false"
user2 := User{
	age: 20
}
println(user2.can_register()) // "true"
```

V não tem classes, mas você pode definir métodos em tipos.
Um método é uma função com um argumento receptor especial.
O receptor aparece em sua própria lista de argumentos entre
a palavra-chave `fn` e o nome do método.
Os métodos devem estar no mesmo módulo que o tipo de receptor.

Neste exemplo, o método `can_register` possui um
receptor do tipo `User` chamado `u`.
A convenção não é usar nomes de receptores como `self` ou` this`,
mas um nome curto, de preferência uma letra.

## Unions

Assim como structs, os unions apoiam a incorporação.

```v
struct Rgba32_Component {
	r byte
	g byte
	b byte
	a byte
}

union Rgba32 {
	Rgba32_Component
	value u32
}

clr1 := Rgba32{
	value: 0x008811FF
}

clr2 := Rgba32{
	Rgba32_Component: {
		a: 128
	}
}

sz := sizeof(Rgba32)
unsafe {
	println('Size: ${sz}B,clr1.b: $clr1.b,clr2.b: $clr2.b')
}
```

Output: `Size: 4B, clr1.b: 136, clr2.b: 0`

O acesso de membro do union deve ser realizado em um bloco "unsafe".

Observe que os argumentos de estrutura incorporados não são
necessariamente armazenados na ordem listada.

## Funções 2

### Funções puras por padrão

As funções V são puras por padrão, o que significa que seus
valores de retorno são uma função de seus
argumentos apenas, e sua avaliação não tem efeitos colaterais (além de I/O).

Isso é conseguido pela falta de variáveis ​​globais e todos os argumentos de função sendo
imutável por padrão, mesmo quando [referências](#referências) são passadas.

V não é uma linguagem puramente funcional.

Há um sinalizador do compilador para habilitar variáveis ​​globais (`-enable-globals`),
mas este é destinado a aplicativos de baixo nível como kernels e drivers.

### Argumentos Mutáveis

É possível modificar os argumentos da função usando a palavra-chave `mut`:

```v
struct User {
	name string
mut:
	is_registered bool
}

fn (mut u User) register() {
	u.is_registered = true
}

mut user := User{}
println(user.is_registered) // "false"
user.register()
println(user.is_registered) // "true"
```

Neste exemplo, o receptor (que é simplesmente o primeiro argumento)
é marcado como mutável,
então `register()` pode mudar o objeto do usuário.
O mesmo funciona com argumentos de não receptores:

```v
fn multiply_by_2(mut arr []int) {
	for i in 0 .. arr.len {
		arr[i] *= 2
	}
}

mut nums := [1, 2, 3]
multiply_by_2(mut nums)
println(nums)
// "[2, 4, 6]"
```

Observe que você deve adicionar `mut` antes de` nums` ao chamar esta função. Isto faz
ficar claro que a função que está sendo chamada modificará o valor.

É preferível retornar valores em vez de modificar argumentos.
A modificação de argumentos só deve ser feita em partes
críticas de desempenho de seu aplicativo
para reduzir as alocações e cópias.

Por esta razão, V não permite a modificação de argumentos
com tipos primitivos (por exemplo, inteiros).
Apenas tipos mais complexos, como arrays e maps, podem ser modificados.

Use `user.register()` ou `user = register(user)`
em vez de `register (user mut)`.

#### Struct update syntax

V torna mais fácil retornar uma versão modificada de um objeto:

```v
struct User {
	name          string
	age           int
	is_registered bool
}

fn register(u User) User {
	return {
		...u
		is_registered: true
	}
}

mut user := User{
	name: 'abc'
	age: 23
}
user = register(user)
println(user)
```

### Número variável de argumentos

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
// usando decomposição de array
a := [2, 3, 4]
println(sum(...a)) // <-- usando prefix ... aqui. output: 9
b := [5, 6, 7]
println(sum(...b)) // output: 18
```

### Funções anônimas e de alta ordem

```v
fn sqr(n int) int {
	return n * n
}

fn cube(n int) int {
	return n * n * n
}

fn run(value int, op fn (int) int) int {
	return op(value)
}

fn main() {
	// Funções podem ser passadas para outras funções
	println(run(5, sqr)) // "25"
	// Funções anônimas podem ser declaradas dentro de outras funções:
	double_fn := fn (n int) int {
		return n + n
	}
	println(run(5, double_fn)) // "10"
	// Funções podem ser transmitidas sem atribuí-las a variáveis:
	res := run(5, fn (n int) int {
		return n + n
	})
	println(res) // "10"
	// Você pode até ter um array/map de funções:
	fns := [sqr, cube]
	println(fns[0](10)) // "100"
	fns_map := map{
		'sqr':  sqr
		'cube': cube
	}
	println(fns_map['cube'](2)) // "8"
}
```

## Referências

```v
struct Foo {}

fn (foo Foo) bar_method() {
	// ...
}

fn bar_function(foo Foo) {
	// ...
}
```

Se um argumento de função for imutável (como `foo` nos exemplos acima)
V pode passar por valor ou por referência. O compilador decidirá,
e o desenvolvedor não precisa pensar nisso.

Você não precisa mais se lembrar se deve passar a estrutura por valor
ou por referência.

Você pode garantir que a estrutura seja sempre passada por referência
adicionando `&`:

```v
struct Foo {
	abc int
}

fn (foo &Foo) bar() {
	println(foo.abc)
}
```

`foo` ainda é imutável e não pode ser alterado. Por isso,
`(mut foo Foo)` deve ser usado.

Em geral, as referências de V são semelhantes aos ponteiros Go e referências C++.
Por exemplo, uma definição de estrutura de árvore genérica seria assim:

```v wip
struct Node<T> {
    val   T
    left  &Node
    right &Node
}
```

## Constantes

```v
const (
	pi    = 3.14
	world = '世界'
)

println(pi)
println(world)
```

As constantes são declaradas com `const`. Eles só podem ser definidos
no nível do módulo (fora das funções).
Os valores constantes nunca podem ser alterados. Você também pode declarar um único
constante separadamente:

```v
const e = 2.71828
```

As constantes V são mais flexíveis do que na maioria das linguagens.
Você pode atribuir valores mais complexos:

```v
struct Color {
	r int
	g int
	b int
}

fn rgb(r int, g int, b int) Color {
	return Color{
		r: r
		g: g
		b: b
	}
}

const (
	numbers = [1, 2, 3]
	red     = Color{
		r: 255
		g: 0
		b: 0
	}
	// evaluate function call at compile-time*
	blue = rgb(0, 0, 255)
)

println(numbers)
println(red)
println(blue)
```
\* WIP - por enquanto, as chamadas de função são avaliadas na inicialização do programa

Variáveis ​​globais normalmente não são permitidas, então isso pode ser muito útil.

### Prefixo de módulo necessário

Ao nomear constantes, `snake_case` deve ser usado. A fim de distinguir constantes
a partir de variáveis ​​locais, o caminho completo para consts deve ser especificado.
Por exemplo, para acessar o PI const, o nome completo `math.pi` deve ser usado fora de` math`
módulo e dentro dele. Essa restrição é relaxada apenas para o módulo `main`
(aquele que contém seu `fn main()`), onde você pode usar o nome não qualificado de
constantes definidas lá, ou seja, `numbers`, em vez de` main.numbers`.

vfmt cuida desta regra, então você pode digitar `println (pi)` dentro do módulo `math`,
e o vfmt irá atualizá-lo automaticamente para `println (math.pi)`.

<!--
Muitas pessoas preferem todas as consts em maiúsculas: `TOP_CITIES`. Isso não funcionaria
bem em V, porque consts são muito mais poderosos do que em outras linguagens.
Eles podem representar estruturas complexas, e isso é usado com bastante frequência, uma vez que
não são globais:

```v oksyntax
println('Top cities: ${top_cities.filter(.usa)}')
```
-->

## Funções builtin

Algumas funções são embutidas como `println`. Aqui está a lista completa:

```v ignore
fn print(s string) // print anything on sdtout
fn println(s string) // print anything and a newline on sdtout

fn eprint(s string) // same as print(), but use stderr
fn eprintln(s string) // same as println(), but use stderr

fn exit(code int) // terminate the program with a custom error code
fn panic(s string) // print a message and backtraces on stderr, and terminate the program with error code 1
fn print_backtrace() // print backtraces on stderr
```

`println` é uma função embutida simples, mas poderosa, que pode imprimir qualquer coisa:
strings, números, matrizes, mapas, estruturas.

```v
struct User {
	name string
	age  int
}

println(1) // "1"
println('hi') // "hi"
println([1, 2, 3]) // "[1, 2, 3]"
println(User{ name: 'Bob', age: 20 }) // "User{name:'Bob', age:20}"
```

<a id='custom-print-of-types' />

## Impressão de tipos personalizados

Se você deseja definir um valor de impressão personalizado para o seu tipo, basta definir um
Método `.str() string`:

```v
struct Color {
	r int
	g int
	b int
}

pub fn (c Color) str() string {
	return '{$c.r, $c.g, $c.b}'
}

red := Color{
	r: 255
	g: 0
	b: 0
}
println(red)
```

## Módulos

Cada arquivo na raiz de uma pasta faz parte do mesmo módulo.
Programas simples não precisam especificar o nome do módulo; nesse caso, o padrão é 'main'.

V é uma linguagem muito modular. A criação de módulos reutilizáveis ​​é incentivada e
muito fácil de fazer.
Para criar um novo módulo, crie um diretório com o nome do seu módulo contendo
Arquivos .v com código:

```shell
cd ~/code/modules
mkdir mymodule
vim mymodule/myfile.v
```
```v failcompile
// myfile.v
module mymodule

// Para exportar uma função, temos que usar `pub`
pub fn say_hi() {
	println('hello from mymodule!')
}
```

Agora você pode usar `mymodule` em seu código:

```v failcompile
import mymodule

fn main() {
	mymodule.say_hi()
}
```

* Os nomes dos módulos devem ser curtos, com menos de 10 caracteres.
* Os nomes dos módulos devem usar `snake_case`.
* Importações circulares não são permitidas.
* Você pode ter quantos arquivos .v quiser em um módulo.
* Você pode criar módulos em qualquer lugar.
* Todos os módulos são compilados estaticamente em um único executável.

### Funções `init`

Se você quiser que um módulo chame automaticamente
algum código de configuração/inicialização quando for importado,
você pode usar uma função do módulo `init`:

```v
fn init() {
	// your setup code here ...
}
```

A função `init` não pode ser pública - ela será chamada automaticamente.
Este recurso é particularmente útil para inicializar uma biblioteca C.

### Módulo Gerenciador de pacote

Brevemente:

```powershell
v [module option] [param]
```

###### module options:

```
   install           Install a module from VPM.
   remove            Remove a module that was installed from VPM.
   search            Search for a module from VPM.
   update            Update an installed module from VPM.
   upgrade           Upgrade all the outdated modules.
   list              List all installed modules.
   outdated          Show installed modules that need updates.
```

Leia mais:

Você também pode instalar módulos já criados por outra pessoa com [VPM](https://vpm.vlang.io/):
```powershell
v install [module]
```
###### Exemplo:
```powershell
v install ui
```

Removendo um módulo com V:

```powershell
v remove [module]
```
###### Exemplo:
```powershell
v remove ui
```

Atualizando um módulo instalado do [VPM](https://vpm.vlang.io/):

```powershell
v update [module]
```
###### Exemplo:
```powershell
v update ui
```

Ou você pode atualizar todos os seus módulos:
```powershell
v update
```

Para ver todos os módulos instalados, pode-se usar:

```powershell
v list
```
###### Exemplo
```powershell
> v list
Installed modules:
  markdown
  ui
```

Para ver todos os módulos instalados e que estão desatualizados, pode-se usar:
outdated          Show installed modules that need updates.
```powershell
v outdated
```
###### Exemplo
```powershell
> v outdated
Modules are up to date.
```

Você também pode adicionar seu módulo ao VPM seguindo as instruções no site https://vpm.vlang.io/new

## Tipos 2

### Interfaces

```v
struct Dog {
	breed string
}

struct Cat {
	breed string
}

fn (d Dog) speak() string {
	return 'woof'
}

fn (c Cat) speak() string {
	return 'meow'
}

// ao contrário de Go e TypeScript, as interfaces de V podem definir campos, não apenas métodos.
interface Speaker {
	breed string
	speak() string
}

dog := Dog{'Leonberger'}
cat := Cat{'Siamese'}

mut arr := []Speaker{}
arr << dog
arr << cat
for item in arr {
	println('a $item.breed says: $item.speak()')
}
```

Um tipo implementa uma interface implementando seus métodos e campos.
Não há declaração explícita de intenção, nem palavra-chave "implements".

#### Casting uma interface

Podemos testar o tipo subjacente de uma interface usando operadores de conversão dinâmica:
```v oksyntax
interface Something {}

fn announce(s Something) {
	if s is Dog {
		println('a $s.breed dog') // `s` é automaticamente "casteada" para `Dog` (smart cast)
	} else if s is Cat {
		println('a $s.breed cat')
	} else {
		println('something else')
	}
}
```
Para mais informações, veja [Dynamic casts](#dynamic-casts).

#### Definições de métodos de interface

Também ao contrário do Go, uma interface pode implementar um método.
Esses métodos não são implementados por structs que implementam essa interface.

Quando uma estrutura é envolvida em uma interface que implementou um método
com o mesmo nome de um implementado por esta estrutura, apenas o método
implementado na interface é chamado.

```v
struct Cat {}

fn (c Cat) speak() string {
	return 'meow!'
}

interface Adoptable {}

fn (a Adoptable) speak() string {
	return 'adopt me!'
}

fn new_adoptable() Adoptable {
	return Cat{}
}

fn main() {
	cat := Cat{}
	assert cat.speak() == 'meow!'
	a := new_adoptable()
	assert a.speak() == 'adopt me!'
	if a is Cat {
		println(a.speak()) // meow!
	}
}
```

### Enums

```v
enum Color {
	red
	green
	blue
}

mut color := Color.red
// V sabe que `color` é uma` Color`. Não há necessidade de usar `color = Color.green` aqui.
color = .green
println(color) // "green"
match color {
	.red { println('the color was red') }
	.green { println('the color was green') }
	.blue { println('the color was blue') }
}
```

A correspondência de enum deve ser exaustiva ou ter um branch `else`.
Isso garante que, se um novo campo enum for adicionado,
ele será tratado em todos os lugares do código.

Os campos Enum não podem reutilizar palavras-chave reservadas.
No entanto, palavras-chave reservadas podem ter escape
com um @.

```v
enum Color {
	@none
	red
	green
	blue
}

color := Color.@none
println(color)
```

Inteiros podem ser atribuídos a campos enum.

```v
enum Grocery {
	apple
	orange = 5
	pear
}

g1 := int(Grocery.apple)
g2 := int(Grocery.orange)
g3 := int(Grocery.pear)
println('Grocery IDs: $g1, $g2, $g3')
```

Output: `Grocery IDs: 0, 5, 6`.

Operações não são permitidas em variáveis ​​enum;
eles devem ser explicitamente convertidos em `int`.

### Sum types

Uma instância do tipo sum pode conter um valor de vários tipos diferentes.
Use a palavra-chave `type` para declarar um tipo de sum:

```v
struct Moon {}

struct Mars {}

struct Venus {}

type World = Mars | Moon | Venus

sum := World(Moon{})
assert sum.type_name() == 'Moon'
println(sum)
```
O método embutido `type_name` retorna o nome do atualmente mantido
modelo.

Com os tipos de sum, você pode construir estruturas recursivas
e escrever um código conciso, mas poderoso, nelas.
```v
// V's binary tree
struct Empty {}

struct Node {
	value f64
	left  Tree
	right Tree
}

type Tree = Empty | Node

// sum up all node values
fn sum(tree Tree) f64 {
	return match tree {
		Empty { f64(0) } // TODO: as match gets smarter just remove f64()
		Node { tree.value + sum(tree.left) + sum(tree.right) }
	}
}

fn main() {
	left := Node{0.2, Empty{}, Empty{}}
	right := Node{0.3, Empty{}, Node{0.4, Empty{}, Empty{}}}
	tree := Node{0.5, left, right}
	println(sum(tree)) // 0.2 + 0.3 + 0.4 + 0.5 = 1.4
}
```

#### Dynamic casts

Para verificar se uma instância de tipo de sum contém um certo tipo, use `sum is Type`.
Para lançar um tipo de sum para uma de suas variantes, você pode usar `sum as Type`:

```v
struct Moon {}

struct Mars {}

struct Venus {}

type World = Mars | Moon | Venus

fn (m Mars) dust_storm() bool {
	return true
}

fn main() {
	mut w := World(Moon{})
	assert w is Moon
	w = Mars{}
	// use `as` to access the Mars instance
	mars := w as Mars
	if mars.dust_storm() {
		println('bad weather!')
	}
}
```

`as` será panic se` w` não contiver uma instância de `Mars`.
Uma maneira mais segura é usar um elenco inteligente.

#### Smart casting

```v oksyntax
if w is Mars {
	assert typeof(w).name == 'Mars'
	if w.dust_storm() {
		println('bad weather!')
	}
}
```
`w` tem o tipo` Mars` dentro do corpo da instrução `if`. Isso é
conhecido como * digitação sensível ao fluxo *.
Se `w` for um identificador mutável,
não seria seguro se o compilador o convertesse de maneira inteligente sem um aviso.
É por isso que você deve declarar um `mut` antes da expressão` is`:

```v ignore
if mut w is Mars {
	assert typeof(w).name == 'Mars'
	if w.dust_storm() {
		println('bad weather!')
	}
}
```
Caso contrário, `w` manteria seu tipo original.
> Isso funciona para variáveis ​​simples e expressões complexas como `user.name`

#### Matching sum types

Você também pode usar `match` para determinar a variante:

```v
struct Moon {}

struct Mars {}

struct Venus {}

type World = Mars | Moon | Venus

fn open_parachutes(n int) {
	println(n)
}

fn land(w World) {
	match w {
		Moon {} // no atmosphere
		Mars {
			// light atmosphere
			open_parachutes(3)
		}
		Venus {
			// heavy atmosphere
			open_parachutes(1)
		}
	}
}
```

`match` deve ter um padrão para cada variante ou ter um branch` else`.

```v ignore
struct Moon {}
struct Mars {}
struct Venus {}

type World = Moon | Mars | Venus

fn (m Moon) moon_walk() {}
fn (m Mars) shiver() {}
fn (v Venus) sweat() {}

fn pass_time(w World) {
    match w {
        // using the shadowed match variable, in this case `w` (smart cast)
        Moon { w.moon_walk() }
        Mars { w.shiver() }
        else {}
    }
}
```

### Type aliases

Para definir um novo tipo `NewType` como um alias para` ExistingType`,
faça `type NewType = ExistingType`. <br/>
Este é um caso especial de declaração [sum type](#sum-types).

### Option/Result types and error handling

Os tipos de opções são declarados com `?Type`:
```v
struct User {
	id   int
	name string
}

struct Repo {
	users []User
}

fn (r Repo) find_user_by_id(id int) ?User {
	for user in r.users {
		if user.id == id {
			// V automatically wraps this into an option type
			return user
		}
	}
	return error('User $id not found')
}

fn main() {
	repo := Repo{
		users: [User{1, 'Andrew'}, User{2, 'Bob'}, User{10, 'Charles'}]
	}
	user := repo.find_user_by_id(10) or { // Option types must be handled by `or` blocks
		return
	}
	println(user.id) // "10"
	println(user.name) // "Charles"
}
```

V combina `Option` e` Result` em um tipo,
então você não precisa decidir qual usar.

A quantidade de trabalho necessária para "atualizar"
uma função para uma função opcional é mínima;
você tem que adicionar um `?` ao ​​tipo de retorno
e retornar um erro quando algo der errado.

Se você não precisa retornar uma mensagem de erro,
você pode simplesmente `return none`
(este é um equivalente mais eficiente de `return error (" ")`).

Este é o mecanismo principal para tratamento de erros em V.
Eles ainda são valores, como em Go,
mas a vantagem é que os erros não podem ser resolvidos,
e tratá-los é muito menos prolixo.
Ao contrário de outras linguagens, V não lida com exceções com blocos `throw/try/catch`.

`err` é definido dentro de um bloco `or` e
é definido para a string que a mensagem foi passada
para a função `error()`. `err` estará vazio se `none` for retornado.

```v oksyntax
user := repo.find_user_by_id(7) or {
	println(err) // "User 7 not found"
	return
}
```

### Handling optionals

Existem quatro maneiras de lidar com um opcional.
O primeiro método é propagar o erro:

```v
import net.http

fn f(url string) ?string {
	resp := http.get(url) ?
	return resp.text
}
```

`http.get` retorna `?http.Response`. Porque `?` Segue a chamada, o
o erro será propagado para o chamador de `f`. Ao usar `?` após uma
chamada de função produzindo um opcional, a função envolvente deve retornar
um opcional também. Se a propagação de erro for usada no `main()`
em vez disso, ele entrará em 'panic', uma vez que o erro não pode ser propagado
mais longe.

O corpo de `f` é essencialmente uma versão condensada de:

```v ignore
    resp := http.get(url) or { return err }
    return resp.text
```

---
O segundo método é interromper a execução mais cedo:

```v oksyntax
user := repo.find_user_by_id(7) or { return }
```

Aqui, você pode chamar `panic()` ou `exit()`, o que interromperá a execução de
todo o programa ou usar uma instrução de fluxo de controle (`return`,` break`, `continue`, etc)
para quebrar do bloco atual.
Observe que `break` e` continue` só podem ser usados ​​dentro de um loop `for`.

V não tem uma maneira de forçar "unwrap" um opcional (como outras linguagens fazem,
por exemplo, o `unwrap()` do Rust ou o `!` do Swift). Para fazer isso, use `ou {panic (err.msg)}`.

---
O terceiro método é fornecer um valor padrão no final do bloco `or`.
Em caso de erro, esse valor seria atribuído em vez disso,
portanto, deve ter o mesmo tipo que o conteúdo da `Option` sendo tratada.

```v
fn do_something(s string) ?string {
	if s == 'foo' {
		return 'foo'
	}
	return error('invalid string') // Could be `return none` as well
}

a := do_something('foo') or { 'default' } // a será 'foo'
b := do_something('bar') or { 'default' } // b será 'default'
println(a)
println(b)
```

---
O quarto método é usar desdobramento `if`:

```v
import net.http

if resp := http.get('https://google.com') {
	println(resp.text) // resp is a http.Response, not an optional
} else {
	println(err)
}
```
Acima, `http.get` retorna um `?Http.Response`. `resp` está apenas no escopo para o primeiro
ramo `if`. `err` está apenas no escopo do branch `else`.

## Generics

```v wip

struct Repo<T> {
    db DB
}

struct User {
	id   int
	name string
}

struct Post {
	id   int
	user_id int
	title string
	body string
}

fn new_repo<T>(db DB) Repo<T> {
    return Repo<T>{db: db}
}

// Esta é uma função genérica. V irá gerá-lo para cada tipo com o qual é usado.
fn (r Repo<T>) find_by_id(id int) ?T {
    table_name := T.name // neste exemplo, obter o nome do tipo nos dá o nome da tabela
    return r.db.query_one<T>('select * from $table_name where id = ?', id)
}

db := new_db()
users_repo := new_repo<User>(db) // returns Repo<User>
posts_repo := new_repo<Post>(db) // returns Repo<Post>
user := users_repo.find_by_id(1)? // find_by_id<User>
post := posts_repo.find_by_id(1)? // find_by_id<Post>
```

Atualmente, as definições de funções genéricas devem declarar seus parâmetros de tipo, mas no
futuro V irá inferir parâmetros de tipo genérico a partir de nomes de tipo de uma única letra em
tipos de parâmetro de tempo de execução. É por isso que `find_by_id` pode omitir `<T>`, porque o
o argumento do receptor `r` usa um tipo genérico `T`.

Outro exemplo:
```v
fn compare<T>(a T, b T) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// compare<int>
println(compare(1, 0)) // Outputs: 1
println(compare(1, 1)) //          0
println(compare(1, 2)) //         -1
// compare<string>
println(compare('1', '0')) // Outputs: 1
println(compare('1', '1')) //          0
println(compare('1', '2')) //         -1
// compare<f64>
println(compare(1.1, 1.0)) // Outputs: 1
println(compare(1.1, 1.1)) //          0
println(compare(1.1, 1.2)) //         -1
```


## Concorrência
### Spawning Concurrent Tasks
O modelo de simultaneidade de V é muito semelhante ao de Go.
Para executar `foo()`, simultaneamente em
um thread diferente, basta chamá-lo com `go foo()`:

```v
import math

fn p(a f64, b f64) { // função comum sem valor de retorno
	c := math.sqrt(a * a + b * b)
	println(c)
}

fn main() {
	go p(3, 4)
	// p será rodado em uma thread paralela
}
```

Às vezes, é necessário esperar até que uma thread paralela termine. Isso pode
ser feito atribuindo um * identificador * à thread iniciada e chamando o método `wait()`
para este identificador mais tarde:

```v
import math

fn p(a f64, b f64) { // função comum sem valor de retorno
	c := math.sqrt(a * a + b * b)
	println(c) // prints `5`
}

fn main() {
	h := go p(3, 4)
	// p() roda em thread paralela
	h.wait()
	// p() definitivamente terminou
}
```

Essa abordagem também pode ser usada para obter um
valor de retorno de uma função que é executada em uma
thread paralela. Não há necessidade de modificar
a própria função para poder chamá-la
simultaneamente.

```v
import math { sqrt }

fn get_hypot(a f64, b f64) f64 { // função comum retornando um valor
	c := sqrt(a * a + b * b)
	return c
}

fn main() {
	g := go get_hypot(54.06, 2.08) // spawn thread
	h1 := get_hypot(2.32, 16.74) //   Faz outro cálculo aqui
	h2 := g.wait() //                 pega o resultado da thread spawned (get_hypot(54.06, 2.08))
	println('Results: $h1, $h2') //   prints `Results: 16.9, 54.1`
}
```

Se houver um grande número de tasks, pode ser mais fácil gerenciá-las
usando uma série de threads.

```v
import time

fn task(id int, duration int) {
	println('task $id begin')
	time.sleep(duration * time.millisecond)
	println('task $id end')
}

fn main() {
	mut threads := []thread{}
	threads << go task(1, 500)
	threads << go task(2, 900)
	threads << go task(3, 100)
	threads.wait()
	println('done')
}

// Output:
// task 1 begin
// task 2 begin
// task 3 begin
// task 3 end
// task 1 end
// task 2 end
// done
```

Além disso, para threads que retornam o mesmo tipo, chamando `wait()`
no array de thread retornará todos os valores calculados.

```v
fn expensive_computing(i int) int {
	return i * i
}

fn main() {
	mut threads := []thread int{}
	for i in 1 .. 10 {
		threads << go expensive_computing(i)
	}
	// Junta todas as tasks
	r := threads.wait()
	println('All jobs finished: $r')
}

// Output: All jobs finished: [1, 4, 9, 16, 25, 36, 49, 64, 81]
```

### Channels
Os canais são a forma preferida de comunicação entre as corrotinas.
Os canais de V funcionam basicamente como
aqueles em Go. Você pode empurrar objetos para um canal
em uma extremidade e estourar objetos na outra extremidade.
Os canais podem ser armazenados em buffer ou sem buffer
e é possível `selecionar` a partir de vários canais.

#### Syntax and Usage
Channels têm o tipo `chan objtype`. Um comprimento de buffer
opcional pode ser especificado como a propriedade `cap`
na declaração:

```v
ch := chan int{} // unbuffered - "synchronous"
ch2 := chan f64{cap: 100} // buffer length 100
```

Os channles não precisam ser declarados como `mut`.
O comprimento do buffer não faz parte do tipo, mas
uma propriedade do objeto de canal individual.
Os canais podem ser passados ​​para corrotinas como
variáveis:

```v
fn f(ch chan int) {
	// ...
}

fn main() {
	ch := chan int{}
	go f(ch)
	// ...
}
```

Os objetos podem ser enviados para os canais
usando o operador '<-'. O mesmo operador pode ser usado para
objetos pop do outro lado:

```v
// crie canais em buffer para que o push não
// bloqueie (se houver espaco no buffer)
ch := chan int{cap: 1}
ch2 := chan f64{cap: 1}
n := 5
// push
ch <- n
ch2 <- 7.3
mut y := f64(0.0)
m := <-ch // pop criando nova variável
y = <-ch2 // pop dentro de uma variável existente
```

Um canal pode ser fechado para indicar que nenhum
outro objeto pode ser empurrado. Qualquer tentativa de
fazer isso, resultará em um 'panic' de tempo de execução
(com exceção de `select` e
`try_push()` - veja abaixo). As tentativas de estourar
retornarão imediatamente se o
o canal associado foi fechado e o buffer está vazio.
Esta situação pode ser
manipulado usando um branch ou (consulte [Handling optionals](#handling-optionals)).

```v wip
ch := chan int{}
ch2 := chan f64{}
// ...
ch.close()
// ...
m := <-ch or {
    println('channel has been closed')
}

// propagate error
y := <-ch2 ?
```

#### Channel Select

O comando `select` permite monitorar vários canais ao mesmo tempo
sem carga de CPU perceptível. Consiste em uma lista
de possíveis transferências e filiais associadas
de declarações - semelhante ao comando [match](#match):
```v wip
import time
fn main () {
  c := chan f64{}
  ch := chan f64{}
  ch2 := chan f64{}
  ch3 := chan f64{}
  mut b := 0.0
  // ...
  select {
    a := <-ch {
        // faça algo com `a`
    }
    b = <-ch2 {
        // faça algo com a variável pré-declarada `b`
    }
    ch3 <- c {
        // faça algo se `c` foi enviado
    }
    > 500 * time.millisecond {
        // faça algo se nenhum channel ficou pronto dentro de 0.5s
    }
  }
}
```

O branch de tempo limite é opcional. Se estiver ausente,
`select` espera por um período ilimitado de tempo.
Também é possível proceder imediatamente se nenhum canal
estiver pronto no momento em que `select` é chamado
adicionando um branch `else {...}`. `else` e `> timeout`
são mutuamente exclusivos.

O comando `select` pode ser usado como uma * expressão * do tipo` bool`
que se torna `falso` se todos os canais forem fechados:
```v wip
if select {
    ch <- a {
        // ...
    }
} {
    // channel foi aberto
} else {
    // channel está fechado
}
```

#### Special Channel Features

Para fins especiais, existem algumas propriedades e métodos integrados:
```v
struct Abc {
	x int
}

a := 2.13
ch := chan f64{}
res := ch.try_push(a) // try to perform `ch <- a`
println(res)
l := ch.len // número de elementos na fila
c := ch.cap // tamanho máximo da fila
is_closed := ch.closed // bool flag - `ch` foi fechado?
println(l)
println(c)
mut b := Abc{}
ch2 := chan Abc{}
res2 := ch2.try_pop(mut b) // try to perform `b = <-ch2`
```

Os métodos `try_push/pop()` retornarão imediatamente com um dos resultados
`.success`,` .not_ready` ou `.closed` - depende se o objeto foi transferido ou
a razão pela qual não.
O uso desses métodos e propriedades na produção não é recomendado -
algoritmos baseados neles estão frequentemente sujeitos
a condições de corrida. Especialmente `.len` e
`.closed` não deve ser usado para tomar decisões.
Use ramos `or`, propagação de erro ou `select`
em vez disso (veja [Syntax and Usage](#syntax-and-usage)
e [Channel Select](#channel-select) acima).

### Objetos Compartilhados

Os dados podem ser trocados entre uma co-rotina e
o thread de chamada por meio de uma variável compartilhada.
Essas variáveis ​​devem ser criadas como `shared` e
passadas para a co-rotina como tal.
A `struct` subjacente contém um * mutex * oculto
que permite o bloqueio de acesso simultâneo
usando `rlock` para somente leitura e `lock`
para acesso de leitura/gravação.

```v
struct St {
mut:
	x int // dado a ser compartilhado
}

fn (shared b St) g() {
	lock b {
		// read/modify/write b.x
	}
}

fn main() {
	shared a := St{
		x: 10
	}
	go a.g()
	// ...
	rlock a {
		// read a.x
	}
}
```
Variáveis 'shared' devem ser 'structs', 'arrays' or 'maps'.

## Decoding JSON

```v
import json

struct Foo {
	x int
}

struct User {
	name string
	age  int
	// Use o atributo `skip` para pular certos campos
	foo Foo [skip]
	// Se o nome do campo está diferente no JSON, ele pode ser especificado
	last_name string [json: lastName]
}

data := '{ "name": "Frodo", "lastName": "Baggins", "age": 25 }'
user := json.decode(User, data) or {
	eprintln('Failed to decode json')
	return
}
println(user.name)
println(user.last_name)
println(user.age)
// Você também pode decodificar JSON arrays:
sfoos := '[{"x":123},{"x":456}]'
foos := json.decode([]Foo, sfoos) ?
println(foos[0].x)
println(foos[1].x)
```

Devido à natureza onipresente do JSON, o suporte
para ele é integrado diretamente no V.

A função `json.decode` leva dois argumentos:
o primeiro é o tipo em que o valor JSON deve ser decodificado e
a segunda é uma string contendo os dados JSON.

V gera código para codificação e decodificação JSON.
Nenhuma 'runtime reflection' é usada. Isso resulta
em um desempenho muito melhor.

## Testing

### Asserts

```v
fn foo(mut v []int) {
	v[0] = 1
}

mut v := [20]
foo(mut v)
assert v[0] < 4
```
Uma instrução `assert` verifica se sua expressão é
avaliada como `true`. Se uma afirmação falhar,
o programa será abortado. Asserts devem ser usados
​​apenas para detectar erros de programação. Quando um
declaração falha, é relatado para * stderr *, e os
valores em cada lado de um operador de comparação
(como `<`, `==`) será impresso quando possível.
Isso é útil para encontrar facilmente um
valor inesperado. As declarações assert podem
ser usadas em qualquer função.

### Test files

```v
// hello.v
module main

fn hello() string {
	return 'Hello world'
}

fn main() {
	println(hello())
}
```

```v failcompile
module main

// hello_test.v
fn test_hello() {
	assert hello() == 'Hello world'
}
```
Para executar o teste acima, use `v hello_test.v`.
Isso irá verificar se a função `hello` está
produzindo a saída correta.
V executa todas as funções de teste no arquivo.

* Todas as funções de teste devem estar dentro
de um arquivo de teste cujo nome termina em `_test.v`.
* Os nomes das funções de teste devem começar com
`test_` para marcá-los para execução.
* As funções normais também podem ser definidas em
arquivos de teste e devem ser chamadas manualmente. Outros
  símbolos também podem ser definidos em arquivos
  de teste, por exemplo tipos.
* Existem dois tipos de testes: externos e internos.
* Os testes internos devem * declarar * seu módulo,
assim como todos os outros .v
arquivos do mesmo módulo. Os testes internos podem
até chamar funções privadas em
o mesmo módulo.
* Os testes externos devem * importar * os módulos
que eles testam. Eles não
têm acesso às funções/tipos privados dos módulos.
Eles podem testar apenas
a API externa/pública que um módulo fornece.

No exemplo acima, `test_hello` é um teste interno,
que pode chamar
a função privada `hello()` porque `hello_test.v`
tem `module main`,
assim como `hello.v`, ou seja, ambos fazem parte
do mesmo módulo. Observe também que
uma vez que `module main` é um módulo regular
como os outros, os testes internos podem
também ser usado para testar funções privadas
nos arquivos .v do seu programa principal.

Você também pode definir funções de teste
especiais em um arquivo de teste:
* `testsuite_begin` que será executado * antes *
de todas as outras funções de teste.
* `testsuite_end` que será executado * após *
todas as outras funções de teste.

Se uma função de teste tiver um tipo de retorno
de erro, qualquer erro propagado falhará no teste:

```
import strconv

fn test_atoi() ? {
	assert strconv.atoi('1') ? == 1
	assert strconv.atoi('one') ? == 1 // teste irá falhar
}
```

#### Rodando os testes

Para executar funções de teste em um arquivo
de teste individual, use `v foo_test.v`.

Para testar um módulo inteiro, use `v test mymodule`.
Você também pode usar `v test .` para testar
tudo dentro de sua pasta atual (e subpastas).
Você pode passar a opção `-stats`
para ver mais detalhes sobre os testes individuais executados.

## Gerenciamento de Memória

V evita fazer alocações desnecessárias em
primeiro lugar, usando tipos de valor,
buffers de string, promovendo um estilo
de código simples e livre de abstração.

A maioria dos objetos (~ 90-100%) são
liberados pelo motor autofree de V: o compilador insere
chamadas gratuitas necessárias automaticamente
durante a compilação. Pequena porcentagem restante
de objetos é liberado por meio da contagem de referência.

O desenvolvedor não precisa alterar nada
em seu código. "Simplesmente funciona", como em
Python, Go ou Java, exceto que não há GC
pesado rastreando tudo ou RC caro para
cada objeto.

### Controle

Você pode tirar vantagem do motor autofree
do V e definir um método `free()` nos
tipos de dados customizados:

```v
struct MyType {}

[unsafe]
fn (data &MyType) free() {
	// ...
}
```

Assim como o compilador libera tipos de dados C
com `free()` do C, ele irá inserir estaticamente
`free()` chamadas para seu tipo de dado no final
do tempo de vida de cada variável.

Para desenvolvedores que desejam ter mais controle
de baixo nível, o autofree pode ser desabilitado com
`-manualfree`, ou adicionando um` [manualfree]` em
cada função que deseja gerenciar sua
memória manualmente. (Veja [attributes](#attributes))._

Nota: neste momento, o autofree está escondido atrás
da flag '-autofree'. Será habilitado por
padrão em V 0.3. Se o autofree não for usado,
os programas V vazarão memória._

### Exemplos

```v
import strings

fn draw_text(s string, x int, y int) {
	// ...
}

fn draw_scene() {
	// ...
	name1 := 'abc'
	name2 := 'def ghi'
	draw_text('hello $name1', 10, 10)
	draw_text('hello $name2', 100, 10)
	draw_text(strings.repeat(`X`, 10000), 10, 50)
	// ...
}
```

As strings não escapam de `draw_text`,
então elas são limpas quando
a função sai.

Na verdade, com o sinalizador `-prealloc`,
as duas primeiras chamadas não resultarão em nenhuma alocação.
Essas duas strings são pequenas,
então V usará um buffer pré-alocado para elas.

```v
struct User {
	name string
}

fn test() []int {
	number := 7 // stack variable
	user := User{} // struct alocado na stack
	numbers := [1, 2, 3] // array alocatodo na heap, será liberado quando a função sair
	println(number)
	println(user)
	println(numbers)
	numbers2 := [4, 5, 6] // array que está sendo devolvido, não será liberado aqui
	return numbers2
}
```

## ORM

(Ainda está em estado alfa)

V tem um ORM embutido (mapeamento objeto-relacional)
que suporta SQLite, MySQL e Postgres,
mas em breve terá suporte para MS SQL e Oracle.

ORM da V oferece uma série de benefícios:

- Uma sintaxe para todos os dialetos SQL.
(Migrar entre bancos de dados se torna muito mais fácil.)
- As consultas são construídas usando a sintaxe de V.
(Não há necessidade de aprender outra sintaxe.)
- Segurança.
(Todas as consultas são automaticamente higienizadas para evitar injeção de SQL.)
- Compilar verificações de tempo.
(Isso evita erros de digitação que só podem ser detectados durante o tempo de execução.)
- Legibilidade e simplicidade.
(Você não precisa analisar manualmente os resultados de uma consulta e
    em seguida, construir manualmente os objetos a partir dos resultados analisados.)

```v
import sqlite

struct Customer {
	// nome da struct tem que ser o mesmo que o da nome da tabela (por enquanto)
	id        int    [primary; sql: serial] // um campo chamdado 'id' de tipo integer deve ser o primeiro campo
	name      string [nonull]
	nr_orders int
	country   string [nonull]
}

db := sqlite.connect('customers.db') ?

// você pode criar tabelas
// CREATE TABLE IF NOT EXISTS `Customer` (`id` INTEGER PRIMARY KEY, `name` TEXT NOT NULL, `nr_orders` INTEGER, `country` TEXT NOT NULL)
sql db {
	create table Customer
}

// select count(*) from Customer
nr_customers := sql db {
	select count from Customer
}
println('number of all customers: $nr_customers')
// V syntax pode ser usada para construir queries
uk_customers := sql db {
	select from Customer where country == 'uk' && nr_orders > 0
}
println(uk_customers.len)
for customer in uk_customers {
	println('$customer.id - $customer.name')
}
// adicionando `limit 1`, dizemos para V que terá, somente, um objeto
customer := sql db {
	select from Customer where id == 1 limit 1
}
println('$customer.id - $customer.name')
// insert um novo customer
new_customer := Customer{
	name: 'Bob'
	nr_orders: 10
}
sql db {
	insert new_customer into Customer
}
```

Para mais exemplos e docs,
veja <a href='https://github.com/vlang/v/tree/master/vlib/orm'>vlib/orm</a>.

## Escrevendo a Documentação

A forma como funciona é muito semelhante ao Go.
É muito simples: não há necessidade de
escrever a documentação separadamente para o seu código,
vdoc irá gerá-lo a partir de docstrings no código-fonte.

A documentação para cada função/tipo/const
deve ser colocada logo antes da declaração:

```v
// clearall clears all bits in the array
fn clearall() {
}
```

O comentário deve começar com o nome da definição.

Às vezes, uma linha não é suficiente para explicar
o que uma função faz, nesse caso, os comentários devem
alcançar a função documentada usando comentários
de uma única linha:

```v
// copy_all recursively copies all elements of the array by their value,
// if `dupes` is false all duplicate values are eliminated in the process.
fn copy_all(dupes bool) {
	// ...
}
```

Por convenção, é preferível que os comentários
sejam escritos no * tempo presente *.

Uma visão geral do módulo deve ser colocada
no primeiro comentário logo após o nome do módulo.

Para gerar a documentação, use o vdoc,
por exemplo `v doc net.http`.

## Ferramentas

### v fmt

Você não precisa se preocupar em formatar
seu código ou definir diretrizes de estilo.
`v fmt` cuida disso:

```shell
v fmt file.v
```

É recomendado configurar seu editor,
de forma que `v fmt -w` execute a cada salvamento.
Uma execução vfmt geralmente é bem barata (leva < 30ms).

Sempre execute `v fmt -w file.v` antes de subir seu código

### Profiling

V tem um bom suporte para traçar o perfil
de seus programas: `v -profile profile.txt run file.v`
Isso produzirá um arquivo profile.txt, que você pode analisar.

O arquivo profile.txt gerado terá linhas com 4 colunas:
a) quantas vezes uma função foi chamada
b) quanto tempo no total uma função levou (em ms)
c) quanto tempo, em média, uma chamada para uma função demorou (em ns)
d) o nome da função v

Você pode classificar na coluna 3 (tempo médio por função) usando:
`sort -n -k3 profile.txt | tail`

Você também pode usar cronômetros para medir
apenas partes do seu código explicitamente:
```v
import time

fn main() {
	sw := time.new_stopwatch({})
	println('Hello world')
	println('Greeting the world took: ${sw.elapsed().nanoseconds()}ns')
}
```

# Tópicos Avançados

## Dumping expressions at runtime
Você pode copiar/rastrear o valor de qualquer expressão V usando `dump (expr)`.
Por exemplo, salve este exemplo de código como `factorial.v` e execute-o com
`v run factorial.v`:
```v
fn factorial(n u32) u32 {
	if dump(n <= 1) {
		return dump(1)
	}
	return dump(n * factorial(n - 1))
}

fn main() {
	println(factorial(5))
}
```
Você vai ter:
```
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: true
[factorial.v:3] 1: 1
[factorial.v:5] n * factorial(n - 1): 2
[factorial.v:5] n * factorial(n - 1): 6
[factorial.v:5] n * factorial(n - 1): 24
[factorial.v:5] n * factorial(n - 1): 120
120
```
Observe que `dump(expr)` rastreará tanto a localização da fonte,
a própria expressão e o valor da expressão.

## Memory-unsafe code

Às vezes, para eficiência,
você pode querer escrever um código de baixo nível que pode potencialmente
corromper a memória ou ser
vulnerável a falhas de segurança. V suporta escrever esse código,
mas não por padrão.

V requer que quaisquer operações potencialmente
inseguras para a memória sejam marcadas intencionalmente.
Marcá-los também indica para quem está lendo o código que pode haver
violações de segurança de memória se houver um erro.

Exemplos de operações potencialmente inseguras para a memória são:

* Pointer aritmética
* Indexação de ponteiro
* Conversão para ponteiro de um tipo incompatível
* Chamar certas funções C, por exemplo `free`,` strlen` e `strncmp`.

Para marcar operações potencialmente
inseguras para a memória, coloque-as em um bloco `unsafe`:

```v wip
// alocar 2 bytes não inicializados e retornar uma referência a eles
mut p := unsafe { malloc(2) }
p[0] = `h` // Error: pointer indexing is only allowed in `unsafe` blocks
unsafe {
    p[0] = `h` // OK
    p[1] = `i`
}
p++ // Error: pointer arithmetic is only allowed in `unsafe` blocks
unsafe {
    p++ // OK
}
assert *p == `i`
```

A prática recomendada é evitar colocar
expressões seguras de memória dentro de um bloco `unsafe`,
para que o motivo do uso de `unsafe`
seja o mais claro possível. Geralmente qualquer código
que você acha que é seguro para a
memória não deve estar dentro de um bloco `unsafe`, então o compilador
pode verificar isso.

Se você suspeita que seu programa viola a segurança da memória, você tem uma vantagem
encontrando a causa: olhe para os blocos `unsafe` (e como eles interagem com
código circundante).

* Nota: Este é um trabalho em andamento.

### Structs com campos de referência

Structs com referências requerem definir
explicitamente o valor inicial para um
valor de referência, a menos que a struct
já defina seu próprio valor inicial.

Referências de valor zero, ou ponteiros nulos,
** NÃO ** serão compatíveis no futuro,
por enquanto, structs de dados como Linked Lists
ou Árvores Binárias que dependem de campos de referência
que podem usar o valor `0`, entendendo que não é seguro, e que pode
causar pânico.

```v
struct Node {
	a &Node
	b &Node = 0 // Auto-initialized to nil, use with caution!
}

// Reference fields must be initialized unless an initial value is declared.
// Zero (0) is OK but use with caution, it's a nil pointer.
foo := Node{
	a: 0
}
bar := Node{
	a: &foo
}
baz := Node{
	a: 0
	b: 0
}
qux := Node{
	a: &foo
	b: &bar
}
println(baz)
println(qux)
```

## sizeof and __offsetof

* `sizeof(Type)` dá o tamanho de um tipo em bytes.
* `__offsetof(Struct, field_name)` fornece o
deslocamento em bytes de um campo de estrutura.

```v
struct Foo {
	a int
	b int
}

assert sizeof(Foo) == 8
assert __offsetof(Foo, a) == 0
assert __offsetof(Foo, b) == 4
```

## Chamando C de V

### Exemplo

```v
#flag -lsqlite3
#include "sqlite3.h"
// See also the example from https://www.sqlite.org/quickstart.html
struct C.sqlite3 {
}

struct C.sqlite3_stmt {
}

type FnSqlite3Callback = fn (voidptr, int, &&char, &&char) int

fn C.sqlite3_open(&char, &&C.sqlite3) int

fn C.sqlite3_close(&C.sqlite3) int

fn C.sqlite3_column_int(stmt &C.sqlite3_stmt, n int) int

// ... você também pode apenas definir o tipo de parâmetro e omitir o prefixo C.
fn C.sqlite3_prepare_v2(&C.sqlite3, &char, int, &&C.sqlite3_stmt, &&char) int

fn C.sqlite3_step(&C.sqlite3_stmt)

fn C.sqlite3_finalize(&C.sqlite3_stmt)

fn C.sqlite3_exec(db &C.sqlite3, sql &char, cb FnSqlite3Callback, cb_arg voidptr, emsg &&char) int

fn C.sqlite3_free(voidptr)

fn my_callback(arg voidptr, howmany int, cvalues &&char, cnames &&char) int {
	unsafe {
		for i in 0 .. howmany {
			print('| ${cstring_to_vstring(cnames[i])}: ${cstring_to_vstring(cvalues[i]):20} ')
		}
	}
	println('|')
	return 0
}

fn main() {
	db := &C.sqlite3(0) // this means `sqlite3* db = 0`
	// passar uma string literal para uma chamada de função C resulta em uma string C, não uma string V
	C.sqlite3_open(c'users.db', &db)
	// C.sqlite3_open(db_path.str, &db)
	query := 'select count(*) from users'
	stmt := &C.sqlite3_stmt(0)
	// Obs: você também pode usar o campo `.str` de uma string V,
	// para obter sua representação terminada em zero no estilo C
	C.sqlite3_prepare_v2(db, &char(query.str), -1, &stmt, 0)
	C.sqlite3_step(stmt)
	nr_users := C.sqlite3_column_int(stmt, 0)
	C.sqlite3_finalize(stmt)
	println('There are $nr_users users in the database.')
	//
	error_msg := &char(0)
	query_all_users := 'select * from users'
	rc := C.sqlite3_exec(db, &char(query_all_users.str), my_callback, voidptr(7), &error_msg)
	if rc != C.SQLITE_OK {
		eprintln(unsafe { cstring_to_vstring(error_msg) })
		C.sqlite3_free(error_msg)
	}
	C.sqlite3_close(db)
}
```

### Passando sinalizadores de compilação C

Adicione as diretivas `#flag` ao topo de seus
arquivos V para fornecer sinalizadores de compilação C como:

- `-I` para adicionar caminhos de pesquisa de arquivos de inclusão C
- `-l` para adicionar nomes de bibliotecas C que você deseja vincular
- `-L` para adicionar caminhos de pesquisa de arquivos de biblioteca C
- `-D` para definir variáveis ​​de tempo de compilação

Você pode (opcionalmente) usar sinalizadores diferentes para alvos diferentes.
Atualmente os sinalizadores `linux`,` darwin`, `freebsd` e` windows` são suportados.

Obs: Cada flag deve ir em sua própria linha (por enquanto)

```v oksyntax
#flag linux -lsdl2
#flag linux -Ivig
#flag linux -DCIMGUI_DEFINE_ENUMS_AND_STRUCTS=1
#flag linux -DIMGUI_DISABLE_OBSOLETE_FUNCTIONS=1
#flag linux -DIMGUI_IMPL_API=
```

No comando de construção do console, você pode usar:
* `-cflags` para passar sinalizadores personalizados para o compilador C de backend.
* `-cc` para alterar o compilador backend C padrão.
* Por exemplo: `-cc gcc-9 -cflags -fsanitize = thread`.

Você pode definir uma variável de ambiente `VFLAGS` em seu terminal para armazenar seu` -cc`
e configurações `-cflags`, ao invés de incluí-los no comando de construção todas as vezes.

### #pkgconfig

Adicionar a diretiva `#pkgconfig` é usada para dizer
ao compilador quais módulos devem ser usados ​​para compilar
e vinculando usando os arquivos pkg-config fornecidos
pelas respectivas dependências.

Contanto que crases não possam ser usados ​​em `#flag` e
processos de spawning não sejam desejáveis ​​para segurança
e por motivos de portabilidade,
V usa sua própria biblioteca pkgconfig que é compatível com o padrão
freedesktop one.

Se nenhuma sinalização for passada,
ele adicionará `--cflags` e` --libs`, ambas as linhas abaixo fazem o mesmo:

```v oksyntax
#pkgconfig r_core
#pkgconfig --cflags --libs r_core
```

Os arquivos `.pc` são pesquisados ​​em
uma lista codificada de caminhos padrão do pkg-config, o usuário pode adicionar
caminhos extras usando a variável de
ambiente `PKG_CONFIG_PATH`. Vários módulos podem ser passados.

### Incluindo código C

Você também pode incluir o código C
diretamente em seu módulo V.
Por exemplo, digamos que seu código C
esteja localizado em uma pasta chamada 'c' dentro da pasta do módulo.
Então:

* Coloque um arquivo v.mod dentro da
pasta de nível superior do seu módulo (se você
criou seu módulo com `v new` você já
tem o arquivo v.mod). Por
exemplo:
```v ignore
Module {
	name: 'mymodule',
	description: 'My nice module wraps a simple C library.',
	version: '0.0.1'
	dependencies: []
}
```


* Adicione estas linhas ao topo do seu módulo:
`` `v oksyntax
#flag -I @ VMODROOT / c
#flag @ VMODROOT / c / implementação.o
#include "header.h"
`` `
Obs: @VMODROOT será substituído por 
V com a * pasta pai mais próxima,
onde existe um arquivo v.mod *.
Qualquer arquivo .v ao lado ou abaixo
da pasta onde o arquivo v.mod está,
pode usar `#flag @ VMODROOT/abc` para
se referir a esta pasta.
A pasta @VMODROOT também é * anexada *
ao caminho de pesquisa do módulo,
assim você pode * importar * outros módulos
em seu @VMODROOT, apenas nomeando-os.

As instruções acima farão com que
V procure um arquivo .o compilado em
seu módulo `pasta/c/implementation.o`.
Se V o encontrar, o arquivo 
será vinculado ao executável principal, que usou o módulo.
Se não o encontrar, V assume que existe um arquivo `@ VMODROOT/c/implementation.c`,
e tenta compilá-lo em um arquivo .o, então o usará.

Isso permite que você tenha um código C,
que está contido em um módulo V, para que sua distribuição seja mais fácil.
Você pode ver um exemplo mínimo completo
para usar o código C em um módulo V wrapper aqui:
[project_with_c_code](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code).
Outro exemplo, demonstrando a passagem de
estruturas de C para V e vice-versa:
[interoperar entre C para V para C](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code_2).

### Tipos C

Strings C terminadas em zero comuns podem
ser convertidas em strings V com
`unsafe {& char(cstring) .vstring()}` ou se você já sabe o comprimento com
`inseguro {& char(cstring) .vstring_with_len(len)}`.

Obs: Os métodos .vstring() e
.vstring_with_len() NÃO criam uma cópia do `cstring`,
então você NÃO deve liberá-lo após chamar o método `.vstring()`.
Se você precisar fazer uma cópia da string C
(algumas APIs libc como `getenv` praticamente exigem isso,
já que eles retornam ponteiros para a memória
libc interna), você pode usar `cstring_to_vstring(cstring)`.

No Windows, as APIs C geralmente retornam
as chamadas strings `wide` (codificação utf16).
Eles podem ser convertidos em strings V com `string_from_wide(&u16(cwidestring))`.

V tem esses tipos para facilitar a
interoperabilidade com C:

- `voidptr` para` void * `do C,
- `& byte` para` byte * `do C e
- `& char` para` char * `do C.
- `&& char` para` char ** `do C

Para converter um `voidptr` para uma
referência V, use` user := &User(user_void_ptr) `.

`voidptr` também pode ser desreferenciado
em uma estrutura V através da conversão:` user := User(user_void_ptr) `.

[um exemplo de um módulo que chama o código C de V](https://github.com/vlang/v/blob/master/vlib/v/tests/project_with_c_code/mod1/wrapper.v)

### Declarações C

Os identificadores C são acessados ​​com
o prefixo `C` da mesma forma que
identificadores são acessados. As funções devem ser declaradas
novamente em V antes que possam ser usadas.
Qualquer tipo C pode ser usado atrás do prefixo `C`,
mas os tipos devem ser declarados novamente em V em
para acessar os membros do tipo.

Para redeclarar tipos complexos, como no seguinte código C:

```c
struct SomeCStruct {
	uint8_t implTraits;
	uint16_t memPoolData;
	union {
		struct {
			void* data;
			size_t size;
		};

		DataView view;
	};
};
```

membros de subestruturas de dados podem ser declarados
diretamente na estrutura de contenção conforme abaixo:

```v
struct C.SomeCStruct {
	implTraits  byte
	memPoolData u16
	// Esses membros fazem parte de subestruturas de dados que atualmente não podem ser representadas em V.
	// Declará-los diretamente dessa forma é suficiente para o acesso.
	// union {
	// struct {
	data voidptr
	size size_t
	// }
	view C.DataView
	// }
}
```

A existência dos membros de dados é informada a V,
e eles podem ser usados ​​sem
recriando exatamente a estrutura original.

Alternativamente, você pode [embed](#embedded-structs) as subestruturas de dados para manter
uma estrutura de código paralela.

## Debugando de código C gerado

Para depurar problemas no código C gerado, você pode passar estes sinalizadores:

- `-g` - produz um executável menos otimizado com mais informações de depuração nele.
    V irá forçar os números de linha dos arquivos .v nos stacktraces, que o
    executável produzirá em 'panic'. Geralmente é melhor passar -g, a menos que
    você está escrevendo código de baixo nível, caso em que use a próxima opção `-cg`.
- `-cg` - produz um executável menos otimizado com mais informações de depuração nele.
	O executável usará números de linha de origem C neste caso. É frequente
    usado em combinação com `-keepc`, para que você possa inspecionar o
    Programa C em caso de pânico, ou para que o seu depurador (`gdb`,` lldb` etc.)
    pode mostrar o código-fonte C gerado.
- `-showcc` - imprime o comando C que é usado para construir o programa.
- `-show-c-output` - imprime a saída que seu compilador C produziu
    enquanto compila seu programa.
- `-keepc` - não exclui o arquivo de código-fonte C gerado após um
    compilação. Além disso, continue usando o mesmo caminho de arquivo,
	para que seja mais estável,
    e mais fácil de manter aberto em um editor / IDE.

Para obter a melhor experiência de depuração,
se você estiver escrevendo um wrapper de baixo nível para um existente
Biblioteca C, você pode passar vários desses sinalizadores ao mesmo tempo:
`v -keepc -cg -showcc yourprogram.v`, então apenas execute o seu depurador (gdb/lldb) ou IDE
no executável produzido `your program`.

Se você quiser apenas inspecionar o código C gerado,
sem mais compilação, você também pode usar o sinalizador `-o` (por exemplo,` -o file.c`).
Isso fará com que V produza o `file.c` e então pare.

Se você quiser ver o código-fonte C gerado para * apenas * uma única função C,
por exemplo `main`, você pode usar:` -printfn main -o file.c`.

Para ver uma lista detalhada de todos os sinalizadores que V suporta,
use `v help`,` v help build` e `v help build-c`.

## Compilação Condicional

### Compile time code

`$` é usado como um prefixo para operações de tempo de compilação.

#### $if
```v
// Suporte para várias condições
$if ios || android {
	println('Running on a mobile device!')
}
$if linux && x64 {
	println('64-bit Linux.')
}
// Usado como expressão
os := $if windows { 'Windows' } $else { 'UNIX' }
println('Using $os')
// $else-$if branches
$if tinyc {
	println('tinyc')
} $else $if clang {
	println('clang')
} $else $if gcc {
	println('gcc')
} $else {
	println('different compiler')
}
$if test {
	println('testing')
}
// v -cg ...
$if debug {
	println('debugging')
}
// v -prod ...
$if prod {
	println('production build')
}
// v -d option ...
$if option ? {
	println('custom option')
}
```

Se você deseja que um `if` seja avaliado em tempo de compilação,
ele deve ser prefixado com um sinal `$`.
No momento, ele pode ser usado para detectar um sistema operacional,
compilador, plataforma ou opções de compilação.
`$if debug` é uma opção especial como `$if windows` ou `$if x32`.
Se estiver usando um ifdef personalizado, você precisa da opção `$if?{}`
e compilar com a opção `v -d`.
Lista completa de opções integradas:
| OS                            | Compilers         | Platforms             | Other                     |
| ---                           | ---               | ---                   | ---                       |
| `windows`, `linux`, `macos`   | `gcc`, `tinyc`    | `amd64`, `arm64`      | `debug`, `prod`, `test`   |
| `mac`, `darwin`, `ios`,       | `clang`, `mingw`  | `x64`, `x32`          | `js`, `glibc`, `prealloc` |
| `android`,`mach`, `dragonfly` | `msvc`            | `little_endian`       | `no_bounds_checking`, `freestanding`    |
| `gnu`, `hpux`, `haiku`, `qnx` | `cplusplus`       | `big_endian`          |
| `solaris` | | | |

#### $embed_file

```v ignore
import os
fn main() {
	embedded_file := $embed_file('v.png')
	os.write_file('exported.png', embedded_file.to_string()) ?
}
```

V pode embutir arquivos arbitrários no executável com o `$ embed_file(<path>)`
chamada de tempo de compilação. Os caminhos podem ser absolutos
ou relativos ao arquivo de origem.

Quando você não usa `-prod`, o arquivo não será incorporado. Em vez disso,
será carregado * pela primeira vez * que seu programa chama `f.data()`
em tempo de execução, tornando
mais fácil mudar em programas de editor externo, sem a necessidade de recompilar
seu executável.

Quando você compila com `-prod`, o arquivo * será embutido dentro * de seu
executável, aumentando seu tamanho binário, mas tornando-o mais independente
e, portanto, mais fácil de distribuir. Neste caso, `f.data()` causará * nenhum IO *,
e sempre retornará os mesmos dados.

#### $tmpl para incorporar e analisar arquivos de modelo V

V tem uma linguagem de modelo simples para modelos de texto e html,
e eles podem facilmente
ser embutido via `$tmpl('path/to/template.txt')`:


```v ignore
fn build() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	return $tmpl('1.txt')
}

fn main() {
	println(build())
}
```

1.txt:

```
name: @name

age: @age

numbers: @numbers

@for number in numbers
  @number
@end
```

output:

```
name: Peter

age: 25

numbers: [1, 2, 3]

1
2
3
```




#### $env

```v
module main

fn main() {
	compile_time_env := $env('ENV_VAR')
	println(compile_time_env)
}
```

V pode trazer valores em tempo de compilação a partir de variáveis ​​de ambiente.
`$env('ENV_VAR')` também pode ser usado no nível superior `#flag` e `#include`:
`#flag linux -I $env('JAVA_HOME')/include`.

### Arquivos específicos do ambiente

Se um arquivo tiver um sufixo específico do ambiente,
ele será compilado apenas para esse ambiente.

- `.js.v` => será usado apenas pelo back-end JS. Esses arquivos podem conter código JS.
- `.c.v` => será usado apenas pelo back-end C. Esses arquivos podem conter código C.
- `.native.v` => será usado apenas pelo back-end nativo de V.
- `_nix.c.v` => será usado apenas em sistemas Unix (não Windows).
- `_${os}.c.v` => será usado apenas no sistema `os` específico.
Por exemplo, `_windows.c.v` será usado apenas ao compilar no Windows, ou com `-os windows`.
- `_default.c.v` => será usado apenas se NÃO houver um arquivo de plataforma mais específico.
Por exemplo, se você tiver `file_linux.c.v` e `file_default.c.v`,
e você está compilando para Linux, então somente `file_linux.c.v` será usado
e `file_default.c.v` será ignorado.

Exemplo mais completo:
main.v:
```v ignore
module main
fn main() { println(message) }
```

main_default.c.v:
```v ignore
module main
const ( message = 'Hello world' )
```

main_linux.c.v:
```v ignore
module main
const ( message = 'Hello linux' )
```

main_windows.c.v:
```v ignore
module main
const ( message = 'Hello windows' )
```

Com o exemplo acima:
- ao compilar para o Windows, você obterá 'Hello windows'
- quando você compila para o Linux, você obterá 'Hello linux'
- ao compilar para qualquer outra plataforma, você obterá o
mensagem não específica 'Hello, world'.

- `_d_customflag.v` => será usado * apenas * se você passar` -d customflag` para V.
Isso corresponde a `$if customflag?{} `, mas para um arquivo inteiro, não apenas um
bloco único. `customflag` deve ser um identificador snake_case, não pode
conter caracteres arbitrários (apenas letras latinas minúsculas + números + `_`).
NB: um postfix combinatório `_d_customflag_linux.c.v` não funcionará.
Se você precisar de um arquivo de sinalizador personalizado,
que tenha código dependente da plataforma, use o
postfix `_d_customflag.v`, e então use o tempo de compilação dependente do plaftorm
blocos condicionais dentro dele, ou seja, `$if linux{}` etc.

- `_notd_customflag.v` => semelhante a _d_customflag.v, mas será usado
* apenas * se você NÃO passar `-d customflag` para V.

## Pseudo variáveis ​​de tempo de compilação

V também dá ao seu código acesso a um conjunto de variáveis ​​de pseudo string,
que são substituídos em tempo de compilação:

- `@FN` => substituído pelo nome da função V atual
- `@METHOD` => substituído com ReceiverType.MethodName
- `@MOD` => substituído com o nome do atual módulo V
- `@STRUCT` => substituído com o nome do atual struct V
- `@FILE` => substituído com o caminho do arquivo de origem V
- `@LINE` => substituído com o número da linha V onde aparece (como uma string).
- `@COLUMN` => substituído com a coluna onde aparece (como uma string).
- `@VEXE` => substituído com o caminho para o compilador V
- `@VEXEROOT`  => será substituído pela * pasta *,
   onde está o executável V (como uma string).
- `@VHASH`  => substituído com o hash de commit encurtado do compilador V (como uma string).
- `@VMOD_FILE` => substituído com o conteúdo do arquivo v.mod mais próximo (como uma string).
- `@VMODROOT` => será substituído pela * pasta *,
   onde está o arquivo v.mod mais próximo (como uma string).

Isso permite que você faça o seguinte exemplo, útil ao depurar/registrar/rastrear seu código:
```v
eprintln('file: ' + @FILE + ' | line: ' + @LINE + ' | fn: ' + @MOD + '.' + @FN)
```

Outro exemplo, é se você deseja incorporar a versão/nome de v.mod * dentro * de seu executável:
```v ignore
import v.vmod
vm := vmod.decode( @VMOD_FILE ) or { panic(err.msg) }
eprintln('$vm.name $vm.version\n $vm.description')
```

## Performance tuning

O código C gerado, geralmente, é rápido o suficiente, quando você compila seu código
com `-prod`.
Existem algumas situações, porém, em que você pode querer dar
dicas adicionais para o compilador, para que ele possa otimizar ainda mais alguns
blocos de código.

Obs: Estes são * raramente * necessários e não devem ser usados, a menos que você
* crie o perfil de seu código * e veja se há benefícios significativos para eles.
Para citar a documentação do gcc: "os programadores são notoriamente ruins em prever
como seus programas realmente funcionam ".

`[inline]` - você pode marcar funções com `[inline]`, então o compilador C irá
tentar embuti-los, o que, em alguns casos, pode ser benéfico para o desempenho,
mas pode afetar o tamanho do seu executável.

`[direct_array_access]` - em funções marcadas com `[direct_array_access]`
o compilador irá traduzir as operações de array diretamente em operações de array C -
omitindo a verificação de limites.
Isso pode economizar muito tempo em uma função que itera
sobre um array, mas ao custo de tornar a função insegura - a menos que
os limites serão verificados pelo usuário.

`if _likely_(bool expression) {` isso sugere que o compilador C, que a
expressão booleana é muito provável que seja verdadeira, então pode gerar assembly
código, com menos chance de erro de predição do branch. No back-end JS,
isso não faz nada.

`if _unlikely_(bool expression) {` semelhante a `_likely_(x)`, mas sugere que
a expressão booleana é altamente improvável. No back-end JS, isso não faz nada.

<a id='Reflection via codegen'>

## Compile-time reflection

Ter suporte JSON integrado é bom, mas V também permite que você crie
serializadores para qualquer formato de dados.
V tem construções `if` e` for` em tempo de compilação:

```v wip
// TODO: não totalmente implementado

struct User {
    name string
    age  int
}

// Note: T deve receber apenas um nome de struct
fn decode<T>(data string) T {
    mut result := T{}
    // compile-time `for` loop
    // T.fields fornece um array de um tipo de metadados de campo
    $for field in T.fields {
        $if field.typ is string {
            // $(string_expr) produz um identificador
            result.$(field.name) = get_string(data, field.name)
        } $else $if field.typ is int {
            result.$(field.name) = get_int(data, field.name)
        }
    }
    return result
}

// `decode<User>` gera:
fn decode_User(data string) User {
    mut result := User{}
    result.name = get_string(data, 'name')
    result.age = get_int(data, 'age')
    return result
}
```

## Limited operator overloading

```v
struct Vec {
	x int
	y int
}

fn (a Vec) str() string {
	return '{$a.x, $a.y}'
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

fn (a Vec) - (b Vec) Vec {
	return Vec{a.x - b.x, a.y - b.y}
}

fn main() {
	a := Vec{2, 3}
	b := Vec{4, 5}
	mut c := Vec{1, 2}
	println(a + b) // "{6, 8}"
	println(a - b) // "{-2, -2}"
	c += a
	println(c) // "{3, 5}"
}
```

A sobrecarga do operador vai contra a filosofia de simplicidade e previsibilidade de V.
Mas como as aplicações científicas e gráficas estão entre os domínios de V,
a sobrecarga do operador é um recurso importante para melhorar a legibilidade:

`a.add(b).add(c.mul(d))` é muito menos legível do que `a + b + c * d`.

Para melhorar a segurança e facilidade de manutenção, a sobrecarga do operador é limitada:

- Só é possível sobrecarregar os operadores `+, -, *, /, %, <, >, ==, !=, <=, >=`.
- `==` e `!=` são gerados automaticamente pelo compilador, mas podem ser sobrescritos.
- Chamar outras funções dentro das funções do operador não é permitido.
- As funções do operador não podem modificar seus argumentos.
- Ao usar os operadores `<` e `==`, o tipo de retorno deve ser `bool`.
- `!=`, `>`, `<=` e `>=` são gerados automaticamente quando `==` e `<` são definidos.
- Ambos os argumentos devem ter o mesmo tipo (assim como todos os operadores em V).
- Operadores de atribuição (`*=`, `+=`, `/=`, etc)
são gerados automaticamente quando os operadores são definidos,
embora devam retornar o mesmo tipo.

## Inline assembly
<!-- ignore because it doesn't pass fmt test (why?) -->
```v ignore
a := 100
b := 20
mut c := 0
asm amd64 {
    mov eax, a
    add eax, b
    mov c, eax
    ; =r (c) as c // output
    ; r (a) as a // input
      r (b) as b
}
println('a: $a') // 100
println('b: $b') // 20
println('c: $c') // 120
```

Para mais exemplos, veja [asm_test.amd64.v](https://github.com/vlang/v/tree/master/vlib/v/tests/assembly/asm_test.amd64.v)

## Traduzindo C para V

TODO: a tradução de C para V estará disponível em V 0.3.

V pode traduzir seu código C para código
V legível por humanos e gerar wrapper V em cima de bibliotecas C.


Vamos criar um programa simples `test.c` primeiro:

```c
#include "stdio.h"

int main() {
	for (int i = 0; i < 10; i++) {
		printf("hello world\n");
	}
        return 0;
}
```

Execute `v translate test.c`, e V gerará `test.v`:

```v
fn main() {
	for i := 0; i < 10; i++ {
		println('hello world')
	}
}
```

Para gerar um wrapper em cima de uma biblioteca C, use este comando:

```bash
v wrapper c_code/libsodium/src/libsodium
```

Isso irá gerar um diretório `libsodium` com um módulo V.

Exemplo de um wrapper libsodium gerado por C2V:

https://github.com/medvednikov/libsodium

<br>

Quando você deve traduzir o código C e quando deve simplesmente chamar o código C de V?

Se você tiver um código C bem escrito e testado,
então, é claro, você pode simplesmente chamar este código C de V.

Traduzir para V oferece várias vantagens:

- Se você planeja desenvolver essa base de código, agora você tem tudo em uma linguagem,
    que é muito mais seguro e fácil de desenvolver do que em C.
- A compilação cruzada torna-se muito mais fácil. Você não precisa se preocupar com isso.
- Não há mais sinalizadores de construção e arquivos de inclusão também.

## Hot code reloading

```v live
module main

import time
import os

[live]
fn print_message() {
	println('Hello! Modify this message while the program is running.')
}

fn main() {
	for {
		print_message()
		time.sleep(500 * time.millisecond)
	}
}
```

Construa este exemplo com `v -live message.v`.

As funções que você deseja recarregar devem ter o atributo `[live]`
antes de sua definição.

No momento, não é possível modificar os tipos durante a execução do programa.

Mais exemplos, incluindo um aplicativo gráfico:
[github.com/vlang/v/tree/master/examples/hot_code_reload](https://github.com/vlang/v/tree/master/examples/hot_reload).

## Cross compilation

Para compilar o seu projeto, basta executar

```shell
v -os windows .
```

ou

```shell
v -os linux .
```

(Cross compiling para macOS, temporariamente, não é possível.)

Se você não tiver nenhuma dependência C,
isso é tudo que você precisa fazer. Isso funciona até
ao compilar aplicativos GUI usando o módulo `ui` ou aplicativos gráficos usando` gg`.

Você precisará instalar o Clang, LLD linker e baixar um arquivo zip com
bibliotecas e incluem arquivos para Windows e Linux. V irá fornecer-lhe um link.

## Cross-platform shell scripts em V

V pode ser usado como uma alternativa ao Bash para escrever
scripts de implantação, construir scripts, etc.

A vantagem de usar V para isso é a simplicidade e previsibilidade da linguagem, e
suporte multiplataforma. Os "scripts V"
são executados em sistemas do tipo Unix e também no Windows.

Use a extensão de arquivo `.vsh`. Isso fará com que todas as funções no `os`
módulo global (para que você possa usar `mkdir()` ao invés de `os.mkdir()`, por exemplo).

Um exemplo de `deploy.vsh`:
```v wip
#!/usr/bin/env -S v run
// O texto acima associa o arquivo V em sistemas semelhantes ao Unix,
// para que possa ser executado apenas especificando o caminho para o arquivo
// uma vez que é tornado executável usando `chmod + x`.

// Remova se compilar/sair, ignore quaisquer erros se não
rmdir_all('build') or { }

// Criar build/, nunca falha porque build/ não existe
mkdir('build') ?

// Move arquivos *.v para build/
result := exec('mv *.v build/') ?
if result.exit_code != 0 {
	println(result.output)
}
// Similar a:
// files := ls('.') ?
// mut count := 0
// if files.len > 0 {
//     for file in files {
//         if file.ends_with('.v') {
//              mv(file, 'build/') or {
//                  println('err: $err')
//                  return
//              }
//         }
//         count++
//     }
// }
// if count == 0 {
//     println('No files')
// }
```

Agora você pode compilar isso como um programa V normal e
obter um executável que você pode implantar e executar
em qualquer lugar:
`v deploy.vsh && ./deploy`

Ou apenas execute-o mais como um script Bash tradicional:
`v execute deploy.vsh`

Em plataformas do tipo Unix, o arquivo pode ser executado
diretamente após torná-lo executável usando `chmod + x`:
`./deploy.vsh`

## Attributes

V possui vários atributos que modificam o comportamento de funções e structs.

Um atributo é uma instrução do compilador especificada dentro de `[]` logo antes de um
declaração function/struct/enum e se aplica somente à declaração a seguir.

```v
// Chamar esta função resultará em um aviso de suspensão de uso
[deprecated]
fn old_function() {
}

// Ele também pode exibir uma mensagem de suspensão de uso personalizada
[deprecated: 'use new_function() instead']
fn legacy_function() {}

// As chamadas desta função serão sequenciais.
[inline]
fn inlined_function() {
}

// A struct a seguir deve ser alocada no heap. Portanto, ele só pode ser usado como uma
// referência (`&Window`) ou dentro de outra referência (`&OuterStruct{Window{...}}`).
[heap]
struct Window {
}

// V não irá gerar esta função e todas as suas chamadas se o sinalizador fornecido for falso.
// Para usar um sinalizador, use `v -d flag`
[if debug]
fn foo() {
}

fn bar() {
	foo() // não será chamado se `-d debug` não for passado
}

// A memória apontada pelos argumentos do ponteiro desta função não será
// liberada pelo GC (se em uso) antes que a função retorne
[keep_args_alive]
fn C.my_external_function(voidptr, int, voidptr) int

// As chamadas para a função a seguir devem ser em blocos unsafe{}.
// Observe que o código no corpo de `risky_business()` ainda será
// verificado, a menos que você também o envolva em blocos `unsafe{}`.
// Isso é útil, quando você quer ter uma função `[unsafe]` que
// tem verificações antes/depois de uma certa operação insegura, que ainda
// beneficie-se dos recursos de segurança do V.
[unsafe]
fn risky_business() {
	// código que será verificado, talvez verificando as pré-condições
	unsafe {
		// código que * não será * verificado, como aritmética de ponteiro,
		// acessando campos de união, chamando outros fns `[unsafe], etc ...
		// Normalmente, é uma boa ideia tentar minimizar o código empacotado
		// unsafe{} tanto quanto possível.
		// Veja também [Memory-unsafe code](#memory-unsafe-code)
	}
	// código que será verificado, talvez verificando as condições de postagem e/ou
	// mantendo invariantes
}

// O mecanismo autofree de V não cuidará do gerenciamento de memória nesta função.
// Você terá a responsabilidade de liberar memória manualmente nele.
[manualfree]
fn custom_allocations() {
}

// Somente para interoperabilidade C, diz a V que a struct a seguir
// é definida com `struct typedef` em C
[typedef]
struct C.Foo {
}

// Usado no código da API Win32 quando você precisa passar a função de retorno de chamada
[windows_stdcall]
fn C.DefWindowProc(hwnd int, msg int, lparam int, wparam int)

// Apenas Windows:
// Se uma biblioteca de gráficos padrão é importada (ex. Gg, ui), a janela gráfica leva
// prioridade e nenhuma janela de console é criada, efetivamente desabilitando as instruções println().
// Use para criar explicitamente a janela do console. Válido apenas antes de main().
[console]
fn main() {
}
```

## Goto

V permite pular incondicionalmente para um rótulo com `goto`. O nome do rótulo deve estar contido
dentro da mesma função da instrução `goto`. Um programa pode `goto` um rótulo fora
ou mais profundo do que o escopo atual. `goto` permite pular a inicialização da variável ou
voltar ao código que acessa a memória que já foi liberada, portanto, requer
`unsafe`.

```v ignore
if x {
	// ...
	if y {
		unsafe {
			goto my_label
		}
	}
	// ...
}
my_label:
```
`goto` deve ser evitado, particularmente quando `for` pode ser usado em seu lugar.
[Labelled break/continue](#labelled-break--continue) pode ser usado para escapar de
um loop aninhado, e aqueles não correm o risco de violar a segurança da memória.

# Apêndices

## Apêndice I: Keywords

V tem 41 keywords reservadas (3 são literais):

```v ignore
as
asm
assert
atomic
break
const
continue
defer
else
embed
enum
false
fn
for
go
goto
if
import
in
interface
is
lock
match
module
mut
none
or
pub
return
rlock
select
shared
sizeof
static
struct
true
type
typeof
union
unsafe
__offsetof
```
Veja também [Tipos](#tipos).

## Apêndice II: Operators

lista de operators para [tipos primitivos](#tipos-primitivos) somente.

```v ignore
+    sum                    integers, floats, strings
-    difference             integers, floats
*    product                integers, floats
/    quotient               integers, floats
%    remainder              integers

~    bitwise NOT            integers
&    bitwise AND            integers
|    bitwise OR             integers
^    bitwise XOR            integers

!    logical NOT            bools
&&   logical AND            bools
||   logical OR             bools
!=   logical XOR            bools

<<   left shift             integer << unsigned integer
>>   right shift            integer >> unsigned integer


Precedência    Operador
    5             *  /  %  <<  >>  &
    4             +  -  |  ^
    3             ==  !=  <  <=  >  >=
    2             &&
    1             ||


Assignment Operators
+=   -=   *=   /=   %=
&=   |=   ^=
>>=  <<=
```
