# One Brainfuck interpreter to rule them all!

Yet another interpreter for Brainfuck dialects:

* Brainfuck
* OokOok
* WoopWoop
* Hodor

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
| Hodor. Hodor! | [         | Jump past the matching Hodor! Hodor. if the cell under the pointer is 0 |
| Hodor! Hodor. | ]         | Jump back to the matching Hodor. Hodor!                                 |

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

```sh
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
```

## Run

```sh
./brainfuck [Hodor|Brainfuck|OokOok] file
```
