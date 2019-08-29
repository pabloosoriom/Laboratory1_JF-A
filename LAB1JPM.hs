import FiniteAutomata

img2dfa :: ImagePixels -> FiniteAutomata ImagePixels Int
img2dfa img = aux 0 0 img fa
where fa :: FiniteAutomata ImagePixels Int
      dfa = FA {
      states = Set.singleton img
      , symbols = imageAlphabet
      , delta = delta'
      , q0 = img
      , f = Set.fromList [0, 1]
      } where delta' :: TransitionFunction state symbol		-- En lugar de simbolos podemos usar enteros?
	      delta' = Map.empty				-- VERIFICAR!
		

aux :: Int -> Int -> ImagePixels -> FiniteAutomata ImagePixels Int -> FiniteAutomata ImagePixels
aux i j img fa@(FA st sy tf as es) = if i == j then fa else u	-- en que comienzan i y j?
where dfa :: FiniteAutomata ImagePixels Int			-- Hay que usar i y j?
      dfa = FA {						-- fa@(FA st sy tf as es)  Aclarar esto. Lo inical: st sy tf as es
      states = Set.union st (Set.fromList imgP) 		--:D more better
      , symbol = sy
      , delta = delta'
      , q0 = as
      , f = es
      } where delta' = 
		addTransition (img, 0, imgP!!0)	$		-- concatenar????????????????
		addTransition (img, 1, imgP!!1) $
		addTransition (img, 2, imgP!!2) $
		addTransition (img, 3, imgP!!3)
		Map.empty					-- Verificar

      imgP = [split img k | k <- [0, 1, 2, 3]]
      i' = i + 4						-- Poner +4 o +1
      j' = j + 4						-- Poner un i
      fas = [aux i' j' image fa | image <- imgP]
      u = joinAu fas


joinAu :: [FA] -> FA
 
split :: ImagePixels -> Int -> ImagePixels
split img k = take (n) imagePixels
where n = length.LISTA/2 




      