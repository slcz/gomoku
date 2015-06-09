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

AI strategy is based on [temporal difference learning](http://en.wikipedia.org/wiki/Temporal_difference_learning). State value of time $t+1$ is used to update a state value at time $t$,

```math
    V(s_t) := V(s_t) + \alpha(r_t + \gamma(V(s_t+1) - V(S_t)))
```
