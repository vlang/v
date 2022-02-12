<div align="center">
<p>
    <img
        style="width: 250px"
        width="250"
        src="https://user-images.githubusercontent.com/17727170/153699135-a63e9644-1a29-4c04-9de3-c9100b06001d.png"
    >
</p>
  
<h1>Pendulum Simulation in V</h1>

You can see the origin implementation among with some benchmarks at
[ulises-jeremias/v-pendulum-simulation](https://github.com/ulises-jeremias/v-pendulum-simulation).

[vlang.io](https://vlang.io) |
[Docs](https://ulises-jeremias.github.io/v-pendulum-simulation) |
[Contributing](https://github.com/ulises-jeremias/v-pendulum-simulation/blob/main/CONTRIBUTING.md)

</div>
<div align="center">

[![Build Status][workflowbadge]][workflowurl]
[![Docs Validation][validatedocsbadge]][validatedocsurl]
[![License: MIT][licensebadge]][licenseurl]

</div>

## Run the Simulations

### Sequential Simulation

```sh
$ v -gc boehm -prod sequential.v
$ ./sequential # execute ./sequential -h for more info
```

### Parallel Simulation

```sh
$ v -gc boehm -prod parallel.v
$ ./parallel # execute ./parallel -h for more info
```

![image](https://user-images.githubusercontent.com/17727170/153718774-1c93b158-aee3-4be1-bb47-fe601fed7336.png)

### Parallel Simulation with Image Worker

```sh
$ v -gc boehm -prod parallel_with_iw.v
$ ./parallel_with_iw # execute ./parallel_with_iw -h for more info
```

![image](https://user-images.githubusercontent.com/17727170/153718769-eabb334d-454f-469f-a51a-14ffe67507de.png)

### Parallel Simulation with Graphic User Interface

```sh
$ v -gc boehm -prod animation.v
$ ./animation # execute ./animation -h for more info
```

### Full Parallel Simulation with Graphic User Interface and Image Output

```sh
$ v -gc boehm -prod full.v
$ ./full # execute ./full -h for more info
```

## Testing

To test the module, just type the following command:

```sh
$ v test .
```

## Benchmark

Check the original repository for tools to run benchmark tests. In there you can execute
the following command to execute benchmark tests to get a full comparison between implementations:

```sh
$ ./bin/run-benchmark-test --help
```

![image](https://user-images.githubusercontent.com/17727170/152750137-98e7c5a3-936b-4bc8-b71a-1b182c0bbf50.png)

[workflowbadge]: https://github.com/ulises-jeremias/v-pendulum-simulation/workflows/Build%20and%20Test%20with%20deps/badge.svg
[validatedocsbadge]: https://github.com/ulises-jeremias/v-pendulum-simulation/workflows/Validate%20Docs/badge.svg
[licensebadge]: https://img.shields.io/badge/License-MIT-blue.svg
[workflowurl]: https://github.com/ulises-jeremias/v-pendulum-simulation/commits/main
[validatedocsurl]: https://github.com/ulises-jeremias/v-pendulum-simulation/commits/main
[licenseurl]: https://github.com/ulises-jeremias/v-pendulum-simulation/blob/main/LICENSE
