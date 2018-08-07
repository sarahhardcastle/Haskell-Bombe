module EnigmaAndMenu where

 import Data.List
 import Data.Char


 

 {- Enigma Simulation
   3 Rotors, simple and steckered
   find the longest Menu in a Crib
   2016 Assignment 2  
 -}
 
 type Cipher = String -- a substitution cypher for A,B..Z
 alphabet :: Cipher
 alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 
 -- ENIGMA
 -- datatypes

 
 -- the real rotors
 -- defined in assignmentHelp
 type Rotor = Cipher  -- an Enigma Rotor
 rotor1 :: Rotor
 rotor1="EKMFLGDQVZNTOWYHXUSPAIBRCJ"
 rotor2 :: Rotor
 rotor2="AJDKSIRUXBLHWTMCQGZNPYFVOE"
 rotor3 :: Rotor
 rotor3="BDFHJLCPRTXVZNYEIWGAKMUSQO"
 rotor4 :: Rotor
 rotor4="ESOVPZJAYQUIRHXLNFTGKDCMWB"
 rotor5 :: Rotor
 rotor5="VZBRGITYUPSDNHLXAWMJQOFECK"

 -- swopping letters for reflector and stecker board
 
 type Letter_Swop =[(Char,Char)]
 
 type Reflector = Letter_Swop
 
 -- the real reflector
 
 reflectorB :: Reflector
             
 reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]
 
 -- Stecker isn't a complete swopping
 type Stecker = Letter_Swop
 
 -- example
 steckerB = [('A','B'), ('C','D'), ('E','F'),('G','H'), ('I','J')]
 
 
 -- offsets for the 3 rotors - L,M and R
 
 type Offset = Int
 type Offsets = (Offset,Offset,Offset)
 
 -- an Enigma
 -- simple or steckered
 
 data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector|
               SteckeredEnigma Rotor Rotor Rotor Reflector Stecker
   deriving (Eq)
 -- examples 
 simpleEnigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB
 steckeredEnigma= SteckeredEnigma rotor1 rotor2 rotor3 reflectorB steckerB              
 
 ------------------------------------------------------               
 {- reflect using the reflector
    x   char to be reflected
    ref reflector
    (p,q) reflector pair with x
 -}
 
 reflect :: Char->Reflector->Char
  
 reflect x ref
  |x==p = q
  |otherwise = p
  where
   [(p,q)] = filter (\(y,z)->(x==y||x==z)) ref --filter to find reflector pair - must be there

 ------------------------------------------------------  
 {- stecker
    stecker a char
    stecker may not be complete - leave char as it is if no match
    x the Char
    sb the Stecker
    (c1,c2) matching pair in stecker
 -}
 
 stecker :: Char->Stecker->Char
 
 stecker x sb
  |null res = x
  |c1 == x = c2
  |otherwise = c1
  where 
   res = filter (\(y,z)->(x==y||x==z)) sb
   [(c1,c2)]=res
                                              
 -------------------------------------------------
 -- advancing the rotors
 -- rotor posnd are (l,m,r)
 
 -- one step
 
 offset_step :: Offsets->Offsets
 
 offset_step (l,m,25) = offsetlm (l,m,0)
 offset_step (l,m,r) = (l,m,r+1)
 
 offsetlm :: Offsets->Offsets
 offsetlm(l,25,0)=offsetl (l,0,0)
 offsetlm (l,m,0)=(l,m+1,0)
 
 offsetl :: Offsets->Offsets
 offsetl (25,0,0)= (0,0,0)
 offsetl (l,0,0)=(l+1,0,0)
 
 -- n steps
 -- needed in breaking Enigma - ass3
 
 offsetN :: Offsets->Int->Offsets
 
 offsetN os 0 = os
 offsetN os n = offsetN (offset_step os) (n-1)
 
 
 -----------------------------------------------------
 -- Enigma encoding
 
 {- encode single Char
    x the char
    e an Enigma
    os offsets triple
    lr,mr,rr the rotors
    ref reflector
    sb steckerboard
 -}
 
 enigmaEncode :: Char->Enigma->Offsets->Char -- encript single Char
 enigmaEncode x e os = enigmaEncodeA x e (offset_step os) -- advance rotors then call A fn to do encoding
 
 -- simple enigma
 
 enigmaEncodeA x (SimpleEnigma lr mr rr ref) (ol,om,or)=
   reverseEncode rr or     -- left rotor returning
   (reverseEncode mr om    -- middle rotor returning
     (reverseEncode lr ol  -- left rotor returning
      (reflect -- fixed reflector
       (encode lr ol -- left rotor forward
         (encode mr om-- mid rotor forward
                (encode rr or x) -- right rotor forward
         )
       )
       ref
      )
     )
    )
  
 -- steckered enigma
 
 enigmaEncodeA x (SteckeredEnigma lr mr rr ref sb) (ol,om,or) = 
    stecker -- stecker the output from the following
     (reverseEncode rr or     -- left rotor returning
      (reverseEncode mr om    -- middle rotor returning
       (reverseEncode lr ol  -- left rotor returning
        (reflect -- fixed reflector
         (encode lr ol -- left rotor forward
          (encode mr om-- mid rotor forward
                 (encode rr or (stecker x sb)) -- right rotor forward
          )
         )
        ref
        )
       )
      )
     )
     sb

{- encode a message
   (h:t) the message
   nos offsets after stepping
   nc encode of h
-}
   
 enigmaEncodeMessage :: String->Enigma->Offsets->String -- encript message 
  
 enigmaEncodeMessage [] (SimpleEnigma _ _ _ _) _ = []       -- 2 versions for 2 constructors
 enigmaEncodeMessage [] (SteckeredEnigma _ _ _ _ _) _ = []
  
 enigmaEncodeMessage (h:t) e os =
    let 
     nos = offset_step os  -- step the rotors
     nc = enigmaEncodeA h e nos -- encode the first Char, without advancing again
    in (nc: enigmaEncodeMessage t e nos) -- encode the rest
 
 ---------------------------------------------------------------------------------------------
 -- ENCODING CHARS, from assignment 1
 -- MOVING CIPHER LEFT, NOT RIGHT, AS REQUIRED FOR ASS3
 -- forward encoding
 -- with an offset
  
 encode :: Cipher->Int->Char->Char -- put Char to be encoded as last arg for partial fn
 
 encode  r n x =  
  let
   p=mod ((alphaPos x) + n)  26  -- position of the given ch x after rotation
  in r!!p
 
 -- reverse encoding
 
 reverseEncode :: Cipher->Int->Char->Char
 
 reverseEncode  r n x =
  let
   p = findPos x r
   q = mod (p-n) 26
  in
   alphabet!!q 


 -- position of a given character in a rotor
  
 findPos :: Char ->Cipher->Int
 
 {- version with explicit recursion
 findPos ch (h:t)
   |(ch==h) = 0
   |otherwise = 1+ (findPos ch t)
 -}
 
 -- with a comprehension
 findPos ch r = 
    head [p |(c,p) <- (zip r [0..25]), c==ch]
    
 -- alphabetic posiiton for an uppercase letter
 -- starting @ 0 for 'A'
 -- ord in Data.Char gives ordering for Chars - 'A' is 65
 
 alphaPos :: Char-> Int
 alphaPos c = (ord c) -65
    
 -----------------------------------------------------------------
 -- CRIBS and MENUS
 
 type Crib = [(Char,Char)] -- ordered list of plain,cipher
 
 -- example
 
 crib1 = zip "WETTERVORHERSAGEBISKAYA"
             "RWIVTYRESXBFOGKUHQBAISE"
 
 -- find index links (x,y) where cipher x == plain y
 -- menu  is list of indices which chain together such links
 -- e.g. for crib 1 [1,0,5,21,12,7,4,3,6,8,18,16,9]
 
 type Menu = [Int]
 
 -------------------------------------------------------------
 -- Bombe follows the longest menu
 -- 2 methods of finding longest menu in a crib:
 -- findMenus (state space search) or 
 -- findMenus3 (menu growing)
 
 longestMenu :: Crib->Menu
 
 longestMenu crib = maximumBy (\m1 m2 ->(compare (length m1) (length m2))) (findMenus crib) -- OR findMenus3
 
 
 ------------------------------------------------------------
 {- findMenus  find all the menus
    state space search-based - open, closed, select from open, add successors to closed, recurse
    successors expand menu m to right
    passive is menus which won't expand to right, but which can still join to others at left.
    mist check successors for circularity
    could probably do without closed alltogether.
    start with menus length 1 ie. [(x,y)] where encript @ x == plain @y
    findLinks extracts this from crib
 -}
 findMenus :: Crib -> [Menu]
 
 findMenus crib = findMenusA (findLinks crib) [] []  -- set up intiial open, passive, closed
 
 -- recursive search
 findMenusA :: [Menu]->[Menu]-> [Menu]-> [Menu]
 
 -- terminate when open empty
 -- returns passive .. menus on closed have been extended
 findMenusA [] passive closed = passive
 
 -- expand first menu on open
 findMenusA open@(m:rm) passive closed
  |not (null nms) = findMenusA (rm++nms) passive (m:closed) -- successors found, so add m to closed
  |otherwise =  findMenusA rm (m:passive) closed -- no successors, add m to passive, recurse with rest of open
  where
   linkedMenus = [mm|mm@(mm1:_)<-(rm++passive),mm1==last m] -- find menus in open or passive m joins to i.e. tail joins to head
   pms = [m++rlm|(_:rlm)<-linkedMenus] -- possible new menus
   nms = [pm|pm<-pms, noDuplicates pm] -- filter to prevent circular chains
   
 --------------------------------------------------------------  
 -- predicate - True if a list has no repeated elements
 -- could use nub
 
 noDuplicates :: Eq a => [a]->Bool
 
 noDuplicates [] = True
 noDuplicates [_] = True
 
 noDuplicates (h:t)
  |(elem h t) = False
  |otherwise = noDuplicates t
  
 ---------------------------------------------------------------------------------
 {- find initial menus of length 2
    these form initial open
    set up plain p and cipher c with indices
    comprehension checks for links & returns index pairs
 -}
 findLinks :: Crib->[Menu]
 
 findLinks crib = [[yi,xi] |(xi,x)<-p,(yi,y)<-c,x==y] 
  where 
   inds = [0..((length crib)-1)]
   (pl,cl)= unzip crib
   p = zip inds pl
   c=  zip inds cl
    
    
 --------------------------------------------------------------------------------
 -- example
 crib1Menus = findMenus crib1
 
 -----------------------------------------------------------------
{- findMenus2
   alternative - growing menus 
   start with initial menu pairs
     join all menus which overlap, i.e. m1= [hm1++olap] joins m2=[olap ++ tm2] to give hm1++olap++tm2
     go for max overlap
     don't join if there's a circularity - a repeated index
     remove m1 & m2 from menu list
     remove new menus which are prefixes of others
     add the new menus to remaining menus, remove duplicated menus
   recurse with new menus ++ remaining menus till no more joining
   
-}

 findMenus2 :: Crib->[Menu]
 
 findMenus2 crib = growMenus (findLinks crib)
 
 -------------------------------------------------------------------
 -- recurse menu expansion until no more joins
 
 growMenus :: [Menu]->[Menu] 
 
 growMenus mlis
  |null joins = mlis -- no more joins, done
  |otherwise = growMenus nlis -- recurse for more
  where
   possJoins = [(mjoin m1 m2)|m1<-mlis,m2<-mlis,m1 /= m2,(overlap m1 m2)>0] -- find menus with overlap & join them
   joins = [m|m<-possJoins, noDuplicates m] -- remove circular menus
   nms = removePrefixes joins -- remove menus which are prefixes of other menus
   nlis  = nub nms    -- remove duplicated menus 

--------------------------------------------------------------    
 {- overlap between 2 lists of Int [i0,i1 ... in] [j0,j1..jm] =   [10,i1,... j0,j1,jm] 
    returns max n for which (drop n l1)== (take n l2)
 -}         

 overlap :: [Int]->[Int]->Int
 
 overlap lis1 lis2 = overlapA lis1 lis2 0
 
 overlapA :: [Int]->[Int]-> Int-> Int
 
 overlapA lis1 lis2 n 
  |n==length lis1 = 0
  |left==right = (length left)
  |otherwise = overlapA lis1 lis2 (n+1)
  where
   left =  (drop n lis1)
   right = (take (length left) lis2)
 
 -------------------------------------------------------
 {- join 2 menus together, allowing for overlap
 -}
 
 mjoin :: [Int]->[Int]->[Int]
 mjoin m1 m2 = (take ((length m1) - n) m1)++m2
  where 
   n=overlap m1 m2
 
-------------------------------------------------------   
 {- removeMenus from a list of menus which are prefixes of others -}
 removePrefixes ::[[Int]]->[[Int]]
 removePrefixes mlis = [m|m<-mlis, null [mp|mp<-mlis, (length mp)>(length m), m == (take (length m) mp)]]
   