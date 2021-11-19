# CSE230-Project
Line Application with Brick Library

# Team

* Megan Chu
* Wei-Cheng Huang
* Dieter Joubert 
* Emma Hogan

# Proposal
We plan to implement the popular game Flappy Bird using Haskell brick. This game involves one moving object, the bird, which is controlled by the user to move strictly vertically up and down in order to clear an ongoing series of gates. The bird object is continuously “falling,” moving downwards at a constant rate unless it is triggered by the user. There is only one type of input control from the user, which triggers the bird object to move a certain amount upwards. The game terminates when the user unsuccessfully clears a gate, and the bird collides with the gate.

We will implement this game as a command line program by having the user use the keyboard to move a bird up and down, in order to pass through continuous sets of gates. One additional feature that we plan to include is a leaderboard, showing the high score for a given session once a session terminates. This screen will also offer the user the option to play again. The score will be measured as the amount of gates passed through before dying (the bird colliding with a gate). 

We plan to use the following packages to help implement the corresponding features:

System.console.haskeline: We will use this package to process user input from the keyboard
System.Random.Shuffle: We will use this package to provide a different random “course” every time the game is played by shuffling a list of possible gates

# Source code overview

todo

# Installation

To build, use `stack`:

```bash
stack build
stack exec flappybird
```

Or both at once:
```bash
stack build && stack exec flappybird
```

# Playing the Game
 todo
