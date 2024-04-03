# Artifact for "An Algebraic Language for Specifying Quantum Networks"

The artifact is a [Haskell][haskell] library `bellkat` plus several examples provided as executables within the same [Haskell][haskell] package.
We provide two build options: [Nix][nix]-based and [Stack][stack]-based, for each we give a [Docker][docker] file that can simplify the setup of the development environment. 
Reproducing the results from the paper can be done in two ways:

  * from a respective development environment ([Nix][nix]- or [Stack][stack]-based) 
  * using an _executable_ [Docker][docker] container providing a [Haskell][haskell] interpreter with `bellkat` already "in scope" (recommended, see "Preparation" section in "Reproducing the results" below)

**Docker note:** please, be aware that `docker run` commands may have to be prefixed with `sudo`,
depending on the docker setup (see details [here][docker-run]).

## Preparing development environment (recommended to skip)

Please, check [Haskell language server documentation][hls] for editor support (optional).

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

## Syntactic differences with the paper:

Bell pairs (e.g., to specify initial states):

 * $A \sim B$ is represented by `"A" ~ "B"`

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

Features:

  * Deciding validity of a policy $p$ on inputs from $\mathcal{N}_0$ and set of valid states $\mathcal{N}$
    (definition 4.10) is represented by `isPolicyValid N0 N p`, where `N` is represented as a predicate $\mathcal{M}(BP) \rightarrow \mathbb{B}$.
  * Deciding equivalence of policies $p$ and $q$ on inputs from $\mathcal{N}_0$ (Theorem 4.4) is
    represented by `arePoliciesEquivalent N0 p q`.

  * Drawing histories of protocols (Figure 3): 

     * `drawHistoriesSVG p` (to create an `.svg` image)
     * `drawHistoriesText p` (to output a textual representation)

## Reproducing the results

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
    # or (for textual version)
    docker run --rm --mount type=bind,source=$(pwd),target=/opt/bellkat -it bellkat:latest\
        examples/P1Text.hs
    ```

  * Stack:

    ```bash
    stack run p1 -- --width 1000 --output P1.svg
    # or (for textual version)
    stack run p1text
    ```

  * Nix:

    ```bash
    cabal run p1 -- --width 1000 --output P1.svg
    # or (for textual version)
    cabal run p1text
    ```

### Example P2 and history in Fig 3 (b)

The protocols are specified in `examples/P2.hs`, history would be saved in `P2.svg`.

  * Docker (recommended)

    ```bash
    docker run --rm --mount type=bind,source=$(pwd),target=/opt/bellkat -it bellkat:latest\
        examples/P2.hs --width 1000 --output P2.svg
    # or (for textual version)
    docker run --rm --mount type=bind,source=$(pwd),target=/opt/bellkat -it bellkat:latest\
        examples/P2Text.hs
    ```

  * Stack:

    ```bash
    stack run p2 -- --width 1000 --output P2.svg
    # or (for textual version)
    stack run p2text
    ```

  * Nix:

    ```bash
    cabal run p2 -- --width 1000 --output P2.svg
    # or (for textual version)
    cabal run p2text
    ```

### Example P3

Perform four checks using `examples/P3.hs` (uses [HSpec][hspec] library, please check its documentation to understand the uses of `describe`, `it` and `shouldBe` within the example):

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

## Writing and testing your own protocols

Most of the relevant documentation is present in `BellKAT.Prelude` module. To see it nicely
formatted **(not for docker)**, you can build documentation using [Haddock][haddock]:

 * Stack: `stack haddock`
 * Nix: `cabal haddock`

In both cases, the output should display the path to `index.html` root of the documentation

### Setting up and running the examples

If you want to work with your own protocols or modify existing ones you have multiple options:

  * modify the existing examples

  * create new examples within BellKAT repository

    1. Creating a new file (say `MyExample.hs`) inside `examples/` (e.g., by copying `examples/P3.hs`)

    2. Tell `stack` or `cabal` about the example by adding

       ```yaml
       my-example:
         dependencies: 
           - bellkat
         main: examples/MyExample.hs
       ```

       to `package.yaml`

    3. Run your example with stack, using `stack run my-example` or `cabal run my-example`

    4. (optional) to enable editor support add
       ```yaml
       - path: "examples/MyExample.hs"
         component: "exe:my-example"
       ```
       to `hie.yaml`

    5. Continue editing `examples/MyExample.hs` as you wish.

  * Use `BellKAT` as a library:

     * **Stack:** see the template in `reuse-templates/stack`
     * **Nix:** see the template in reuse-templates/nix

[nix]: https://nixos.org/download
[flakes]: https://nixos.wiki/wiki/Flakes#Other_Distros.2C_without_Home-Manager
[stack]: https://docs.haskellstack.org/en/stable/
[pango]: https://pango.gnome.org/
[cairo]: https://www.cairographics.org
[zlib]: https://www.zlib.net/
[glib]: https://docs.gtk.org/glib/
[ncurses]: https://invisible-island.net/ncurses/
[docker]: https://docs.docker.com/
[docker-run]: https://docs.docker.com/engine/reference/run/#commands-and-arguments
[haskell]: https://www.haskell.org/
[hls]: https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor
[HSpec]: https://hspec.github.io/
[haddock]: https://haskell-haddock.readthedocs.io/en/latest/
