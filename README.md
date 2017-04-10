# lkn. [![Build Status](https://travis-ci.org/lkn-org/lkn.svg?branch=master)](https://travis-ci.org/lkn-org/lkn) [![Coverage Status](https://coveralls.io/repos/github/lkn-org/lkn/badge.svg)](https://coveralls.io/github/lkn-org/lkn)

**lkn** is an attempt to write a playable 2D MMORPG with Elixir and an
actor-based model.

## Getting Started

The **lkn** codebase is kept under [pijul](https://pijul.com) vcs. To get the
code, you have to use `pijul clone`:

```
pijul clone https://nest.pijul.com/lthms/lkn
```

We use `mix` under the hood, but for simplicity, the main commands are
abstracted away thanks to a `Makefile`. Then:

```
# compile the project from scratch
make
# build doc, generate test coverage, execute the tests
make test
```

For convenience, we also have a [git mirror](https://github.com/lkn-org/lkn).
