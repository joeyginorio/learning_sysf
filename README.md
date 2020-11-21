To run, first compile Test.hs with

```bash
ghc -rtsopts -O2 Run.hs
```

Now you can use Test to run different learning specifications with

```bash
./Run "fileName" +RTS -sstderr
```
The RTS and ssterr flags aren't strictly necessary but give useful runtime information, like how long it takes for the program to run and memory usage. I've included examples of learning specifications in the test folder, you would run one with

```bash
./Run "test/poly_id.f" +RTS -sstderr
```
Currently, the output is printed in System F. So making sure the specification learned the "right" thing requires some care. 
