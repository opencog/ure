# Unified Rule Engine

opencog | singnet
------- | -------
[![CircleCI](https://circleci.com/gh/opencog/ure.svg?style=svg)](https://circleci.com/gh/opencog/ure) | [![CircleCI](https://circleci.com/gh/singnet/ure.svg?style=svg)](https://circleci.com/gh/singnet/ure)

The Unified Rule Engine, URE for short, is a term rewriting engine for
OpenCog. It can be used to implement any logic. As of today it is used
for [PLN](https://wiki.opencog.org/w/Probabilistic_logic_networks),
the [Pattern Miner](https://wiki.opencog.org/w/Pattern_miner) and
[Relex2Logic](https://wiki.opencog.org/w/Category:RelEx2Logic).

The strengths of the URE are

* Reads/writes knowledge directly from/to the
  [AtomSpace](https://wiki.opencog.org/w/AtomSpace).
* It is generic, can be used to implement any logic, even higher order
  logics with some limitations.
* Comes with a powerful control mechanism to speed up reasoning.

# This version of the URE is no longer maintained

Effort has been shifted to [Hyperon Chainer](https://github.com/trueagi-io/chaining).

## Building and Installing

### Prerequisites

To build the URE you need to first build and install:
* The [AtomSpace](https://wiki.opencog.org/w/AtomSpace)
* The [term unifier](https://github.com/opencog/unify)

See
[Building-and-installing-the-AtomSpace](https://github.com/opencog/atomspace#building-and-installing)
for more information.

### Building URE

Be sure to install the pre-requisites first!
Perform the following steps at the shell prompt:
```
    cd ure
    mkdir build
    cd build
    cmake ..
    make -j
```
Libraries will be built into subdirectories within build, mirroring
the structure of the source directory root.

### Unit tests

To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make -j test
```
Tests can be run in parallel as well:
```
    make -j test ARGS=-j4
```

### Install

After building, you must install the URE.
```
    sudo make install
```

## Examples

Examples can be found in this repository under

[URE examples](examples/ure)

And

[PLN examples](https://github.com/opencog/opencog/tree/master/examples/pln)

for PLN in particular.

## More info

The primary documentation for the URE is here:

* [URE wiki](https://wiki.opencog.org/w/URE)
* [URE README.md](opencog/ure/README.md)
