# haskell-bfi
A brainf*ck interpreter written in Haskell

## Installation
First of all, you need to setup the Haskell building and execution environment (like GHC and Stack).

To construct it, I recommend [GHCup](https://www.haskell.org/ghcup/), because it's very isolated (it constructs the all in the environment in `${HOME}/.ghcup`, so if you want to uninstall it, you just remove the directory) and can hold multiple versions of GHC at a time.

See https://www.haskell.org/ghcup/ and follow the instructions on the page.

## Usage
First, clone this repository at anywhere you want.

```console
$ git clone https://github.com/yskuniv/haskell-bfi.git
```

Then, change the directory and run `stack build`.

```console
$ cd haskell-bfi/
$ stack build
```

Now you can run the interpreter with `stack run FILE` like the following.

```console
$ stack run samples/helloworld.bf
Hello, world!
$
```

If you want to see the help, just you run `stack run -- --help`.

```console
$ stack run -- --help
haskell-bfi - A brainf*ck interpreter implemented in haskell.

Usage: bfi FILE
  Run FILE

Available options:
  -h,--help                Show this help text
$
```

If you want to install the command for using it, you can do it by `stack install`. By default, maybe it's installed at `${HOME}/.local/bin`. Now you can run the command like the following.

```console
$ ${HOME}/.local/bin/bfi samples/helloworld.bf
Hello World!
$
```

Or if you want to run with just `bfi`, adjust your `PATH` like the following, or write the export part in your `~/.bashrc`.

```console
$ export PATH="$HOME/.local/bin:$PATH"
$ bfi samples/helloworld.bf
Hello World!
$
```
