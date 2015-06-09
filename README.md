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
        V(t) ← V(t) + α(r(t) + λ(V(t+1) - V(t)))
```
Where _V(t)_ is the value of state at time _t_, α is learning rate, λ is the decay factor that credits the reward to previous moves and _r(t)_ is the instant reward get from time _t_. Suppose the terminal state is specified by _T_, the end game reward _r(T)_ is 1 if the game is won, 0.5 when tie and 0 when loss. The reward is then back propagated to the previous states, subject to the decay factor λ. For all intermediate moves, _r(t)_ are set to 0, stands for no instant rewards.

Since the game is played by two parties in turn, the mover always plays to maximize its own value and minimize the opponents value. The actual value function uses minmax strategy, i.e.,
```math
        V(t) ← 1 - V(t) - λ(V(t+1) - V(t)), when t ≠ T
        V(T) ← 1
```

AI training happens offline after each game, since only when the reward (win/loss/tie) is known. Each step is trained by a 3 layers neural network, using target $V(t+1) - V(t)$. Input features are gathered by scanning the board for all 5 neighbouring positions (horizontally, vertically and diagnoally), looking for locations occupied with only movers' stones (and/or empty slots). In particular, the following patterns are recognized as input features,
```
...oo
..o.o
..oo.
..ooo
.o..o
.o.o.
.o.oo
.oo.o
.ooo.
.oooo
o...o
o..oo
o.o.o
o.ooo
oo.oo
ooooo
```
