# gomoku
Gomoku game written in haskell and based on gloss library. It is played on 15x15 board, though the size of the board can be changed.

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

AI strategy is based on [temporal difference learning](http://en.wikipedia.org/wiki/Temporal_difference_learning). State value of time _t+1_ is used to update a state value at time _t_,
```math
    V(t) := V(t) + α(r(t) + γ(V(t+1) - V(t)))
```
