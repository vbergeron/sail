# Sail

## Functional and Declarative Dockerfile Templating

### Rationale

Writing parametrized Dockerfiles relies too often on using `--build-arg` and then embedding scripts inside the container. Sail aims to provide a mode concise and elegant way to solve this issue, alongside allowing to mutialize best practice cross dockerfiles.

### What is Sail ?

Sail is spacialized programing language targeting Dockerfiles as a target. 
It provides :

* Container builds as a first class citizen
* An algebra to manipulate build instructions
* Templates as first class citizen
* Functions and variables
* Module imports
* Sensible scoping rules (hello bash)

