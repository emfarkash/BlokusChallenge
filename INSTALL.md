# Instructions to Install and Build System

## Table of Contents

- [Dependencies](#dependencies)
- [Start Game](#start-game)
- [Run Tests](#tests)
- [Generate Documentation](#documentation)

## Dependencies

### Prerequisites

Requires that Node.js is installed. You can install Node.js at https://nodejs.org/en/download/
<br />

To install all OCaml and Node.js dependencies, run the following command in the terminal from the base project directory:

```sh
make deps
```

## Start Game

To run the game, run the following command in the terminal from the base project directory:

```sh
make start
```

Then visit http://localhost:3000 on a modern browser (if it isn't opened automatically)

## Run Tests

To run test for the OCaml system, run the following command in the terminal from the base project directory:

```sh
make test
```

## Generate Documentation

To generate documentation for the OCaml modules, run the following command in the terminal from the base project directory:

```sh
make docs
```

The documentation files will be located at <br />
~./backend/\_build/default/\_doc/\_html/api@26bb1931b3ad/Api <br />
and <br />
~./backend/\_build/default/\_doc/\_html/lib@26bb1931b3ad/Lib
