# A Brainfuck interpreter to rule them all!

Yet another interpreter for Brainfuck dialects:

* Brainfuck
* OokOok
* WoopWoop
* Hodor
* Buffalo

With a common backend, so that no new parser is needed to create a new dialect.

## What is this Hodor language ?

Surprisingly, it seems that Hodor is *turing complete* since he is capable of implementing a dialect of Brainfuck...
Here are the instructions:

| Hodor         | Brainfuck | Description                                                             |
| ------------- |:---------:| -----------------------------------------------------------------------:|
| Hodor. Hodor? | >         | Move the pointer to the right                                           |
| Hodor? Hodor. | <         | Move the pointer to the left                                            |
| Hodor. Hodor. | +         | Increment the memory cell under the pointer                             |
| Hodor! Hodor! | -         | Decrement the memory cell under the pointer                             |
| Hodor! Hodor. | .         | Output the character signified by the cell at the pointer               |
| Hodor. Hodor! | ,         | Input a character and store it in the cell at the pointer               |
| Hodor! Hodor? | [         | Jump past the matching Hodor? Hodor! if the cell under the pointer is 0 |
| Hodor? Hodor! | ]         | Jump back to the matching Hodor! Hodor?                                 |

## Example

Here is how Hodor would say "Hello World!":
```
Hodor. Hodor? Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor! Hodor? Hodor? Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor? Hodor! Hodor! Hodor? Hodor! Hodor? Hodor.
Hodor! Hodor. Hodor. Hodor? Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor! Hodor? Hodor? Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor?
Hodor! Hodor! Hodor? Hodor! Hodor? Hodor. Hodor. Hodor. Hodor! Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor! Hodor. Hodor! Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor! Hodor. Hodor. Hodor? Hodor. Hodor? Hodor. Hodor? Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor! Hodor? Hodor? Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor? Hodor! Hodor! Hodor? Hodor! Hodor? Hodor. Hodor! Hodor.
Hodor. Hodor? Hodor. Hodor? Hodor. Hodor? Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor! Hodor? Hodor? Hodor. Hodor. Hodor.
Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor. Hodor? Hodor! Hodor! Hodor? Hodor! Hodor? Hodor. Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor.
Hodor? Hodor. Hodor? Hodor. Hodor? Hodor. Hodor? Hodor. Hodor! Hodor. Hodor. Hodor. Hodor. Hodor. Hodor. Hodor.
Hodor! Hodor. Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor.
Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor! Hodor!
Hodor! Hodor. Hodor. Hodor? Hodor. Hodor? Hodor. Hodor. Hodor! Hodor.
```

## Build

First you must have `stack` [installed](http://docs.haskellstack.org/en/stable/install_and_upgrade/).
On recent Ubuntu (16.04) it can be installed with:

```sh
apt-get install haskell-stack
```

Then compiling is very easy:
```sh
stack build
```

## Run

```sh
stack exec hodor -- [Hodor|Brainfuck|OokOok|WoopWoop|Buffalo] file
```
