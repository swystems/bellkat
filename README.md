# Artifact for "An Algebraic Language for Specifying Quantum Networks"

The artifact provides two packaging options: [Nix][nix]-based, [Stack][stack]-based, and
[Docker][docker]-based.

## Preparing development environment

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

## Building the artifact

### Nix

```bash
cabal build
```

### Stack

```bash
stack build
```


[nix]: https://nixos.org/download
[flakes]: https://nixos.wiki/wiki/Flakes
[stack]: https://docs.haskellstack.org/en/stable/
[pango]: https://pango.gnome.org/
[cairo]: https://www.cairographics.org
[zlib]: https://www.zlib.net/
[glib]: https://docs.gtk.org/glib/
[ncurses]: https://docs.gtk.org/glib/
