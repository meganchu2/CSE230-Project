# CSE230-Project
Command Line Application with Brick Library

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

# Milestone 2

* What is the architecture of your application (the key components)?
  - FlappyBird.hs - backend to control game logic (moving Game from current state to next)
  - UI.hs - game display rendering and interaction
  - dependencies:
    - Brick
    - Linear.V2 (for Coord, Barrier)
    - Random (for generating barriers) 
    - Control (Lens)
* What challenges (if any) did you have so far and how did you solve them?
  - generating random coord/barrier for testing
  - how to test game state/display?
  - how to represent barrier to easily check if bird dies (used list of coords, if bird coord same as one from barrier)
  - understanding brick library (looked at packages on hoogle)
* Do you expect to meet your goals until the deadline?
  - yes, we are on track to finishing main story points
* If not, how will you modify your goals?
  - we may not have time to finish additional features (such as nicer looking bird, different difficulties, saving highest score, etc...)

# Source code overview

* `Main.hs`: todo
* `FlappyBird.hs`: todo
* `UI.hs`: todo
* `Test.hs`: todo

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

# Testing

Add new tests in `tests/Test.hs`. Tests are functions beginning with `prop_` and must return type `Bool` or `Property`.

To run tests, use `stack`:

```bash
stack test
```

# Playing the Game
 
 * `spacebar` starts the game 
 * `spacebar` makes the bird jump
 * `spacebar` resets the game after "game over"
 * `q` or `esc` may be pressed at any time to exit the game

