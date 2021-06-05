# V Trabalho em progresso

***Este documento descreve recursos que ainda não foram implementados.
Por favor, consulte [docs.md](https://github.com/vlang/v/blob/master/doc/docs.md)
para o estado atual de V***

## Índice

* [Concorrência](#concorrência)
    * [Declarações de Variáveis](#declarações-de-variáveis)
	* [Forças](#forças)
	* [Fraquezas](#fraquezas)
	* [Compatibilidade](#compatibilidade)
	* [Automatic Lock](#automatic-lock)
	* [Channels](#channels)

## Concorrência

### Declaração de Variáveis

Objetos que deveriam ser usados ​​para trocar dados entre
as corrotinas devem ser declaradas com cuidado especial. Exatamente um dos seguintes
4 tipos de declaração devem ser escolhidos:

```v ignore
a := ...
mut b := ...
shared c := ...
atomic d := ...
```

- `a` é declarado como * constante * que pode ser passado para
  outras corrotinas e ler sem limitações. Contudo
  não pode ser alterado.
- `b` pode ser acessado para leitura e escrita, mas apenas de uma
  corrotina. Essa corrotina * possui * o objeto. Uma variável `mut` pode
  ser passado para outra corrotina (como receptor ou argumento de função em
  a instrução `go` ou por meio de um channel), mas então a propriedade é passada,
  também, e apenas a outra corrotina pode acessar o objeto. <sup> 1 </sup>
- `c` pode ser passado para corrotinas e acessadas
  * concorrentemente *. <sup> 2 </sup> A fim de evitar disputas de dados, é necessário
  ser bloqueado antes que o acesso possa ocorrer e desbloqueado para permitir o acesso a
  outras corrotinas. Isso é feito por uma das seguintes estruturas de bloco:
  ```v ignore
  lock c {
      // read, modify, write c
      ...
  }
  ```
  
  ```v ignore
  rlock c {
      // read c
      ...
  }
  ```
  Várias variáveis ​​podem ser especificadas: `lock x, y, z {...}`.
  Eles são desbloqueados na ordem oposta.
- `d` pode ser passado para corrotinas e acessado * simultaneamente *,
  também. <sup> 3 </sup> Nenhum bloqueio é necessário neste caso, no entanto
  Variáveis ​​`atomic` só podem ser inteiros de 32/64 bits (ou ponteiros)
  e o acesso é limitado a um pequeno conjunto de linguagens predefinidas que têm
  suporte de hardware nativo.

Para ajudar a tomar a decisão correta, a tabela a seguir resume as
capacidades diferentes:

|                           | *default* | `mut` | `shared` | `atomic` |
| :---                      |   :---:   | :---: |  :---:   |  :---:   |
| write access              |           |   +   |     +    |    +     |
| concurrent access         |     +     |       |     +    |    +     |
| performance               |    ++     |  ++   |          |    +     |
| operações sofisticadas    |     +     |   +   |     +    |          |
| structured data types     |     +     |   +   |     +    |          |

### Forças
#### default
- muito rápido
- acesso ilimitado de diferentes corrotinas
- fácil de lidar

#### `mut`
- muito rápido
- fácil de lidar

#### `shared`
- acesso simultâneo de diferentes corrotinas
- o tipo de dados pode ser uma estrutura complexa
- acesso sofisticado possível (várias instruções dentro de um bloco `lock`)

#### `atomic`
- acesso simultâneo de diferentes corrotinas
- razoavelmente rápido

### Fraquezas
#### default
- somente leitura

#### `mut`
- acesso apenas de uma co-rotina de cada vez

#### `shared`
- bloquear/desbloquear são lentos
- moderadamente difícil de manusear (precisa de blocos `lock`)

#### `atomic`
- limitado a números inteiros únicos (máx. 64 bits) (e ponteiros)
- apenas um pequeno conjunto de operações predefinidas possíveis
- muito difícil de manusear corretamente

<sup> 1 </sup> A corrotina proprietária também irá liberar o espaço de memória usado
para o objeto quando ele não for mais necessário.
<sup> 2 </sup> Para objetos `shared`, o compilador adiciona código para contagem de referência.
Assim que o contador chegar a 0, o objeto será automaticamente liberado.
<sup> 3 </sup> Uma vez que uma variável `atomic` tem apenas alguns bytes de tamanho,
a alocação seria uma sobrecarga desnecessária. Em vez do compilador
criar um global.

### Compatibilidade
Fora dos blocos `lock`/`rlock`, os argumentos da função devem em geral
combinar - com a exceção familiar de que objetos declarados `mut` podem ser
usado para chamar funções que esperam argumentos imutáveis:

```v ignore
fn f(x St) {...}
fn g(mut x St) {...}
fn h(shared x St) {...}
fn i(atomic x u64) {...}

a := St{...}
f(a)

mut b := St{...}
f(b)
go g(mut b)
// `b` não deve mais ser acessado aqui

shared c := St{...}
h(shared c)

atomic d &u64
i(atomic d)
```

Dentro de um bloco `lock c {...}`, `c` se comporta como um `mut`,
dentro de um bloco `rlock c {...}`, como um imutável:
```v ignore
shared c := St{...}
lock c {
    g(mut c)
    f(c)
    // chamada para h() não permitida dentro do bloco `lock`
    // já que  () irá bloquear a própria `c`
}
rlock c {
    f(c)
    // chamada para g() ou h() não permitida
}
```

### Automatic Lock
Em geral, o compilador irá gerar uma mensagem de erro quando um objeto `shared`
é acessado fora de qualquer bloco `lock`/`rlock` correspondente.
No entanto, em casos simples e óbvios, o bloqueio/desbloqueio necessário
pode ser gerado automaticamente para operações `array`/`map`:

```v ignore
shared a := []int{cap: 5}
go h2(shared a)
a << 3
// tenha em mente que `h2()` pode mudar `a` entre essas declarações
a << 4
x := a[1] // não necessariamente `4`

shared b := map[string]int{}
go h3(shared b)
b['apple'] = 3
c['plume'] = 7
y := b['apple'] // não necessariamente `3`

// iteração sobre os elementos
for k, v in b {
    // pares k/v alterados simultaneamente podem ou não ser incluídos
}
```

Isso é útil, mas uma vez que outras corrotinas podem acessar o `array`/`map`
simultaneamente entre as instruções bloqueadas automaticamente, os resultados
às vezes são surpreendentes. Cada declaração deve ser vista como uma única
transação que não está relacionada ao anterior ou seguinte.
Portanto - mas também por motivos de desempenho - é frequentemente
melhor agrupar instruções coerentes consecutivas em um bloco `lock` explícito.
