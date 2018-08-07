module Bombe_Testing where
 import Bombe
 import EnigmaAndMenu
--  import whatever you need 
 
 {- testing for assignment3-}
 
 {- TEST1: a contrived example for which the solution should be found quickly
    if the enigma is set up as described in the case study and the original message is
 -}
 
 p1 = "AIDEGHMC"
 
 -- and the encripted message is
 
 x1 = "IDEGHMCL"
 
 -- and the crib is the whole message i.e.
 
 c1 = zip p1 x1
 

 {- and the Start position is 0
    then you should find a solution for rotor positions (0,0,0)  
    - i.e. enigmaEncode was called with (0,0,0) so the rotors were at (0,0,1) when
           the first character was encoded
    the stecker discovered will be -}
 
 st1 = [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]
 
 -- you may also find an erroneous solution with the initial rotor positions (25,25,25) 

 {- to check you can decode x1 to give p1 with 
    enigmaEncodeMessage x1 (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB st1 (0,0,0)
 -}   
 ------------------------------------------------------------------------------
-- TEST2

 -- the message is known to start with
 
 dcs_header = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
 
 -- the encoded message is
 
 c2 = "RCQRSVHNYQHLVKLELFYSYCCLMKHUFXMVYVREFLHZOLRCBRHWPQDUONZWOGRTYKAUW"
 
 crib2 = zip dcs_header c2
 
 -- note that you may get a partial decode, as in assignment 1, but you should be able to guess the remaining letters
 
 -----------------------------------------------------------------------
 --TEST3

 -- the message starts with 
 ht = "TURINGBOMBEHASKELLSIMULATIONSTOP"
 
 -- the encoded message is
 
 c3= "FNWGDVEHEHJXCGOTOHQLELJOAGABOIDLXIGKFISZUZCAQNUWKXUMSWTYMBIDZF"

 crib3 = zip ht c3
 
 pextra = "ONTHETWELVTHDAYOFCHRISTMASMYLECTURERGAVETOMEADICKISHCHALLENGE"
 extra = "HWHYBANIKFUUXIZXMVADOBBEVKNXBGRTCJSBIDURVXSPRCOIPROVEKYVTYDRESBEBOABJXAGGOEHMIHNMTPO"
 crib4 = zip pextra extra               
                
              
