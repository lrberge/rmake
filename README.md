

### rmake

Welcome to the rmake's project!

The objective of this R package is to facilitate the management of research projects.

How does it work?

1. you create independent chunks of code (code pieces that can be run indepedently from each other)
1. to create chunks of code, use header comments (comments ending with four hashes) beginning with an equal sign: 

    - ex: `#### = this is a chunk ####`

    - the code of a chunk starts at the chunk comment and ends at the beginning of a new chunk
1. place all your functions in files that are sourced thanks to the `.Rprofile` file 
    - note: the `.Rprofile` is a file that is always run at the start of an R session
1. run `rmake::rmake()` and only the code that needs to be run will be run

That's all you need to do! The dependencies (files input/output and functions) are automatically found by the algorithm.

##### Limitations

- currently some dependencies may be missed based on the functions you use => but there is work in progress to make the dependency-deucting mechanism more robust

