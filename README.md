# Cube
Functional model of Rubik's cube 2x2

If you'd like to test my prog, you should run
$ ghc -O2 Main.hs
or with -rtsopts flag to use run-time options

Then do

$ ./Main
and enter the cube.
Cube is represented by 8 numbers, separated with whitespaces. Every number must be one of the lists: [10, 11, 12], [20, 21, 22],
..., [80, 81, 82]. Be sure, that you entered one number from every list, and sum of remainders can be divided by three.
For example, 80 21 70 60 31 42 50 12
When you think that there are enough solutions, press ^C or wait, until program steps over all sequences.

GLHF!
