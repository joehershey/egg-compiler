"Egg" Compiler

This project contains code that covers each step a compiler must complete in order to generate code, including lexical analysis, syntax analysis, and code 
optimization/generation. Written code is tokenized with JLex, translated into a context free grammar with Java CUP, structured into an Abstract Syntax Tree(AST)
via an expansive network of classes, name-analyzed (insuring variables are declared, only once, and not accessed out of scope), type-checked (making sure variables
contain the right type, and expressions makes sense i.e "24 + true - 'compiler' * var1" is invalid), and finally the tree is translated into MIPS assembly code to 
be executed. 

The project was certainly a challenge, and was very daunting at first, but forced me to see the value of incremental code development and testing. In addition, 
it sharpened my Java skills a bunch, especially with understanding class inheritence, abstract classes, mutability, and working with tons of interdependent pieces
of code/classes. I've also learn more about the benefits of makefiles, command-line tricks, and tree parsing. 

The compiler was coded incrementally throughout the duration of "Introduction to Programming Languages and Compilers", a UW-Madison Course I took in Fall 2020. Each addition to the
program was new class project (six in total, this being the last). All code in the .java files was at one point done in previous projects or during this project, 
however, any code that was not needed for the final implementation (generating assembly code) was rewritten by TAs and/or Professor Aws Albarghouthi for the giving
students who could not finish the previous project a chance at a clean slate. Thus, a majority of the code is not technically written by me, but in previous projects
was done by me.

