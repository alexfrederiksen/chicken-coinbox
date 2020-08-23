# Coinbox Hero in Chicken Scheme
A chicken scheme game inspired by Coinbox Hero using a custom Reactive-ECS engine

## Compiling
To compile a desktop executable: run `chicken-csc desktop.scm`

## Keys
* `Q`: quit the program
* `Escape`: break into a Chicken REPL where you may modify the code's internals (:
* `WASD`: move player 1
* `Arrows`: move player 2

## Code architecture
The game uses a very custom engine written also in Chicken Scheme. This engine includes a full Entity-Component interface layer along with an event system inspired by push-based reactive programming paradigms. Entities are not directly stored, instead components are stored in hash maps with entity id's attached to them. This provides a convenient and efficient way to iterate all entites having particular components, which is done quite a lot in game programming. Everything done so far is really just a proof-of-concept, there is still some distance before I can call this a complete game engine.

## Current Tasks
* Obtain sound. This will likely happen by porting SDL_mixer over to Chicken 5 to comfortably use it.
* Finish networking layer. This feels 90% complete, however needs to be thoroughly tested and implemented into the collision system (which honestly needs to be rewritten).
* Rewrite collision system. Right now, it uses a naive per-axis collision method, which in practive functions alright. However will be pain as things get more complex (like entity frames changing sizes).
* Reorganize sections into seperate files, and start building modules.

## Contributions
Feel free to contribute!
