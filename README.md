
This folder contains code needed for the lab project of Compilers - FIB, UPC.

Details and documentation can be found at the Compilers Lab project page
  http://www.cs.upc.edu/~padro/CL/practica

While in the Lab environment, use this command in case of a lib problem:
```
cat $HOME/assig/cl/tcshrc.CL-GRAU.antlr4 >> ~/.tcshrc && source ~/.tcshrc
```
 
Debug mode:
```bash
./asl ../examples/jp_genc_06.asl > a.t
```
```bash
../tvm/tvm a.t --debug < ../examples/jp_genc_06.in 2> debug.out
```