### Bombe implementation using Haskell###

Use this to help decipher messages encoded by the enigma machine.
This project was completed as part of my degree, and is expanded from code written by the lecturer.

##### Bombe.hs #####

This contains the code for finding the offsets and steckerboard.

run
```
breakEnigma [crib]
```
with the name of a crib (a list of pairs of decoded and encoded letters) to return a set of offsets and a steckerboard.

##### bombeTesting16.hs #####

This contains cribs that can be used with breakEnigma.
