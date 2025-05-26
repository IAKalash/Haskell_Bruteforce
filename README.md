To start the bruteforce clone the repo and use

```bash
./main.exe +RTS -N10"
``` 

You can print the number of cores you're going to use after "-N".
To compile the project via ghc use
```bash
ghc -O2 -threaded main.hs +RTS -N10
```