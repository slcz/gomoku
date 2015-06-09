# gomoku
Gomoku game written in haskell and based on gloss library. 15x15 board. AI is based on Temporal difference neural networks.

```
Usage: gomoku [OPTION...]
  -d USEC            --delay=USEC              delay microseconds
  -D WIDTH,HEIGHT    --dimension=WIDTH,HEIGHT  board dimension width,height
  -w CONNECTED       --wincond=CONNECTED       number of connected moves to win
  -p PARAMETER_FILE  --theta=PARAMETER_FILE    parameter file name
  -m MODE,MODE       --mode=MODE,MODE          playing mode ([human|ai],[human|ai])
)
```

![screenshot] (https://github.com/slcz/gomoku/blob/master/board.png)
