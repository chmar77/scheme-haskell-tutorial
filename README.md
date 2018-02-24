# Source Code for learning to create seventyseven Intepreter with Haskell


# Todo
- Showcase data types
- Showcase function declaration
- Showcase various operation (map, + - , cons, etc in stdlib)
- Run a few useful programs

## Data Types


## Pattern Marching
[define [max first . rest] [fold [lambda [old new] [if [> old new] old new]] first rest]]