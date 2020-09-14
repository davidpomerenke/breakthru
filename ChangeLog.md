# Changelog for breakthru

This document serves as an intermediate report on what I have done, in what order, to reconstruct how the software has evolved. It is not really a changelog. 

## Choosing frameworks

I have done previous work on games and search [in Javascript](https://github.com/davidpomerenke/checkers) and in Elm (unpublished). For this project, I choose to use Elm for the frontend again (because it is such a nice language, and perfectly suited for the frontend). I choose not to use Elm for the search implementations, because, being bound to the browser, it lacks good support for parallelism. Instead, I choose to use Haskell for the search implementations, as it is almost as nice as Elm with regard to preventing errors, and because it has good support for parallelism.

## Defining the game

The game is defined in the terms of the state-actions-result-utility framework. There are two approaches of where to put the information about whose turn it is and who thus has to maximize utility at the moment:
    1. It could be put in the algorithms. This is common for MiniMax / Alpha-Beta algorithms (or at least I believe to remember so): The algorithms themselves constantly alternate between maximizing and minimizing. There are two disadvantages:
      - It is a conceptual challenge for the programmer that she has to code the alternation into the algorithms. This is a possible source of errors I would like to avoid.
      - Breakthru does not have alternating moves, but players can either do a double move or a single move, with constraints. One could model double moves as a single move, but this complicates the representation of a move, which makes it harder to develop user interfaces.
    These disadvantages are very acceptable, though.
    2. It could be put inside the state-action-result-utility representation of the breakthru game. This is what I choose to do. The main motivation is the following advantage which I am speculating about:
      - With this representation it will hopefully be easily possible to use algorithms from the domain of problem-solving, such as best-first-search (A* search), without modifying them too much.

## Making a user interface

Haskell server, Elm client. The user interface does not have much logic on its own but relies to the highest possible amount on the responses from the server. This makes it relatively easy to change representations. (JSON encoding is tedious, though.)

## Making a random agent

## Making a heuristic

## Making a max agent

## Making an agent competition benchmark

## Evaluating the results

## Making a minimax agent

## Creating a state database ----- estimating the branching factor
