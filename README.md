# Artifact for "An Algebraic Language for Specifying Quantum Networks"

The artifact is a [Haskell][haskell] library `bellkat` plus several examples provided as executables within the same [Haskell][haskell] package.
We provide two build options: [Nix][nix]-based and [Stack][stack]-based, for each we give a [Docker][docker] file that can simplify the setup of the development environment. 
Reproducing the results from the paper can be done in two ways:

  * from a respective development environment ([Nix][nix]- or [Stack][stack]-based) 
  * using an _executable_ [Docker][docker] container providing a [Haskell][haskell] interpreter with `bellkat` already "in scope" (recommended)

## Preparing development environment (recommended to skip)

### Nix

  * install [Nix][nix] package manager
  * enable [Nix flakes][flakes]
  * **to enter environment run** `nix develop` from the artifact's root

For convenience, we provide `Dockerfile.nixdev` with the environment already set up:

```bash
docker build --tag bellkat:nixdev --file Dockerfile.nixdev . # build the image
docker run --rm --interactive --tty bellkat:nixdev # to enter the environment
```

### Stack

  * install [Stack][stack]
  * install the following extra dependencies: 

     * [Pango][pango] 1.50.6
     * [Cairo][cairo] 1.21.0
     * [Zlib][zlib] 1.3
     * [Glib][glib] 2.72
     * [Ncurses][ncurses] 6.3

     Those can be installed on ubuntu as follows:

     ```bash
     apt-get install libz-dev libtinfo-dev libcairo-dev libpango1.0
     ```

For convenience, we provide `Dockerfile.stackdev` with the environment already set up:

```bash
docker build --tag bellkat:stackdev --file Dockerfile.stackdev . # build the image
docker run --rm --interactive --tty bellkat:stackdev # to enter the environment
```

## Building the artifact (recommended to skip)

### Nix

```bash
cabal build
```

### Stack

```bash
stack build
```

## Reproducing the results

## Syntactic differences with the paper:

Basic actions:

  * $cr\langle X \rangle$ is represented by `create "X"`
  * $tr\langle X \rightarrow Y \sim Z \rangle$ is represented by `trans "X" ("Y", "Z")`
  * $sw\langle X \sim Y @ Z \rangle$ is represented by `swap "Z" ("X", "Y")`
  * $di\langle X \sim Y\rangle$ is represented by `distill ("X", "Y")`

Operations:

  * sequential composition is represented by `<>`
  * parallel composition $||$ is represented by `<||>`
  * iteration $p^\ast$ is represented by `star p`

Tests:

  * checking absence $[\{\{X \sim Y\}\}]$ is represented by `test ("X" /~? "Y")`
  * checking presence $\{\{X \sim Y\}\} \blacktriangleright \{\{X \sim Y\}\}$ is represented by `test ("X" ~~? "Y")`

### Preparation

  * Docker (recommended): change to artifact's root and create the container by running

    ```bash
    docker build --tag bellkat:latest .
    ```

    **Note** all the docker commands below use bind mount to handle input and output files.

  * Stack: change to the artifact root
  * Nix: change to the artifact root and run `nix develop`

### Example P1 and history in Fig 3 (a)

The protocols are specified in `examples/P1.hs`, history would be saved in `P1.svg`.

  * Docker (recommended)

    ```bash
    docker run --rm --mount type=bind,source=$(pwd),target=/opt/bellkat -it bellkat:latest\
        examples/P1.hs --width 1000 --output P1.svg
    ```

  * Stack:

    ```bash
    stack run p1 --width 1000 --output P1.svg
    ```

  * Nix:

    ```bash
    cabal run p1 --width 1000 --output P1.svg
    ```

### Example P2 and history in Fig 3 (b)

The protocols are specified in `examples/P2.hs`, history would be saved in `P2.svg`.

  * Docker (recommended)

    ```bash
    docker run --rm --mount type=bind,source=$(pwd),target=/opt/bellkat -it bellkat:latest\
        examples/P2.hs --width 1000 --output P2.svg
    ```

  * Stack:

    ```bash
    stack run p2 --width 1000 --output P2.svg
    ```

  * Nix:

    ```bash
    cabal run p2 --width 1000 --output P2.svg
    ```

### Example P3

Perform four checks using `examples/P3.hs` (uses [HSpec][hspec] library):

  * check that the protocol always creates a $A \sim E$ Bell pair
  * check that the protocol does not always creates a $A \sim C$ Bell pair
  * check that 1 qubit memory at location $A$ are _not_ enough
  * check that 3 qubits memory at location $A$ are not enough
  * check that 2 qubits at $A$ and 4 qubits at $D$ are enough

The first two are related _reachability property_ (discussed on line 942 of the paper), while the rest are related to  _memory requirements_ (discussed on line 943 of the paper):


  * Docker (recommended)

    ```bash
    docker run --rm --mount type=bind,source=$(pwd),target=/opt/bellkat -it bellkat:latest\
        examples/P3.hs
    ```

  * Stack:

    ```bash
    stack run p3
    ```

  * Nix:

    ```bash
    cabal run p3
    ```

[nix]: https://nixos.org/download
[flakes]: https://nixos.wiki/wiki/Flakes
[stack]: https://docs.haskellstack.org/en/stable/
[pango]: https://pango.gnome.org/
[cairo]: https://www.cairographics.org
[zlib]: https://www.zlib.net/
[glib]: https://docs.gtk.org/glib/
[ncurses]: https://invisible-island.net/ncurses/
[docker]: https://docs.docker.com/
[haskell]: https://www.haskell.org/
[HSpec]: https://hspec.github.io/
