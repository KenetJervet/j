# j
Jump to commonly used directories

## Installation

```bash
$ stack install
```

And then add the following code to your .bashrc/.zshrc, etc, depending on your shell

### Zsh or Bash

```bash
j () {
	eval `.j $@`
}
```

### Others

TODO

## Usage

### List all jumpers

```bash
kj@DCHORUS2 ~ $ j
lab -> ~/Lab
p -> ~/Lab/personal
n -> ~/Lab/Netis
t -> ~/Lab/tospio
c -> ~/Chest
kj@DCHORUS2 ~ $ 
```

### Jump

```bash
kj@DCHORUS2 ~ $ j p
kj@DCHORUS2 ~/Lab/personal $ 
```

### Add a jumper (or overwrite an existing one)

```bash
kj@DCHORUS2 ~/Lab/personal $ j -s log /var/log
kj@DCHORUS2 ~/Lab/personal $ j log
kj@DCHORUS2 /var/log $ 
```

### Delete a jumper

```bash
kj@DCHORUS2 ~ $ j -u log
kj@DCHORUS2 ~ $ j log
No such jumper: log
kj@DCHORUS2 ~ $ 
```

### Print a jumper destination

Might be useful as a quoted expression

```bash
kj@DCHORUS2 ~ $ j -p c
~/Chest
kj@DCHORUS2 ~ $ 
```

## Notes

The jumpers are saved as `~/.j`. It's a simple csv file of k/v pairs. You can easily modify it with your favorite editor as you wish.
```
