module Bombe where
 import EnigmaAndMenu
 import Data.Maybe
 {-------------------------------------2.1--------------------------------}
 {- breakEnigma :: Crib -> Maybe (Offsets, Stecker)
   - finds menu
   - call breakEnigma' with crib, menu and offsets (0,0,0) -}
 breakEnigma :: Crib -> Maybe (Offsets, Stecker)
 breakEnigma crib = breakEA crib (longestMenu crib) (0,0,0) simpleEnigma
  
 breakGivenEnigma :: Crib -> Enigma -> Maybe (Offsets, Stecker)
 breakGivenEnigma crib simEnigma = breakEA crib (longestMenu crib) (0,0,0) simEnigma

 
 {- breakEnigma' :: Crib -> Menu -> ??Stecker?? -> Offsets -> Maybe (Offsets, Stecker)-}
 breakEA :: Crib -> Menu -> Offsets -> Enigma -> Maybe (Offsets, Stecker)
 breakEA crib menu offsets enigma
  | newStecker == Nothing && offsets == (25,25,25) = Nothing 
  | newStecker == Nothing = breakEA crib menu (offset_step offsets) enigma
  | otherwise = Just (offsets, (fromJust newStecker)) 
   where newStecker = findStecker crib menu [(fst (crib!!(menu!!0)),'A')] offsets enigma
 
 {-------------------------------------2.2--------------------------------}
 {- findStecker :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
   - search for sb that matches the crib with particular offsets
   - given one-pair initial Stecker [(x,y)]
   - if contradiction found, find sb recursively calls self with with [(x,y+1)] until all 26 have been tried/one worked                  -}
 findStecker :: Crib -> Menu -> Stecker -> Offsets -> Enigma -> Maybe Stecker
 findStecker crib menu sb offsets (SimpleEnigma r1 r2 r3 ref)
  | last sb == (fst (crib!!(head menu)),'Z') = newStecker
  | newStecker == Nothing = findStecker crib menu [((fst (head sb)),(succ (snd (head sb))))] offsets (SimpleEnigma r1 r2 r3 ref)
  | enigmaEncodeA (fst (head crib)) (SteckeredEnigma r1 r2 r3 ref (fromJust newStecker)) (offset_step offsets) == snd (head crib) = newStecker
  | otherwise = findStecker crib menu [((fst (head sb)),(succ (snd (head sb))))] offsets (SimpleEnigma r1 r2 r3 ref)
   where newStecker = followMenu crib menu offsets (SteckeredEnigma r1 r2 r3 ref sb)

   
 {-------------------------------------2.3--------------------------------}
 {- followMenu :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
   - head(menu) = (p,c), steckered pair (p,q), q encodes to r at that place with the offsets
   - steckerAdd (r,c)-}
 followMenu :: Crib -> Menu -> Offsets -> Enigma -> Maybe Stecker
 followMenu _ [] _ (SteckeredEnigma _ _ _ _ sb) = Just sb
 followMenu crib menu offsets (SteckeredEnigma r1 r2 r3 ref sb) 
  | newStecker == Nothing = Nothing 
  | otherwise = followMenu crib (tail menu) offsets (SteckeredEnigma r1 r2 r3 ref (fromJust newStecker))
  where newStecker = steckerAdd ((snd (crib!!(head menu))), (enigmaEncodeA (fst (crib!!(head menu))) (SteckeredEnigma r1 r2 r3 ref sb) (offsetN offsets ((head menu)+1)))) sb
   


 
 {-------------------------------------2.4--------------------------------}
 {-Stecker already in pair, return stecker. 
   Both letters not in sb - return new sb with pair added. 
   One/both letters already in sb - return nothing-}
 steckerAdd :: (Char, Char) -> Stecker -> Maybe Stecker
 steckerAdd (a,b) sb
  | a==b = Just sb
  | filter (==(a,b)) sb /= [] || filter (==(b,a)) sb /= [] = Just sb
  | checkChar a sb && checkChar b sb = Just ((a,b):sb) 
  | otherwise = Nothing
 
 checkChar :: Char -> Stecker -> Bool
 checkChar letter sb
  | filter ((==letter).fst) sb /= [] = False 
  | filter ((==letter).snd) sb /= [] = False
  | otherwise = True
 {-returns true if letter is nowhere in stecker)-}

{-test :: [Maybe Stecker]-}
 rotorList :: [Rotor]
 rotorList = [rotor1,rotor2,rotor3,rotor4,rotor5]
