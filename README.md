quibble: neural network code reformatting
=========================================

Quibble will use neural networks to learn code formatting style,
using a rigorous parsing / pretty printing layer to ensure the
NN doesn't change the AST meaning.

## Prerequisites

1. Install Torch
Make sure to install torch following the instructions from [torch.ch](http://torch.ch/docs/getting-started.html), not from someplace else. The installer scripts should work. If you want GPU support, make sure to install CUDA beforehand, and make sure the torch installer picks it up.

2. Follow the rest of the char-rnn [install instructions](https://github.com/karpathy/char-rnn)

## Sampling a checkpoint file

If there is a checkpoint file, run (in the char-rnn directory)

```sh
th sample.lua checkpoint-file -temperature 0.6 -gpuid -1 > output.js
```

The temperature can usefully be between 0 and 1. If the checkpoint file has been computed on a GPU, it has to be sampled on a GPU (`-gpuid` >= 0). Pass in a seed with `-primetext "..."`. 

## Training

Use the instructions for [char-rnn](https://github.com/karpathy/char-rnn) for training, except: You can restart training from a checkpoint using `-checkpoint`.
