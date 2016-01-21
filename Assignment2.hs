module HaskellFinance where
import Data.List
import Dist

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel y0 p n = Dist . last . take (n + 1) $ iterate (concatMap (dr p)) [(y0, 1)]
  where dr p (y0,prob) = [(wins y0 p, prob * 1/4), (lose y0 p, prob * 3/4)]

-- lose = 3/4 chance of lose our x
lose :: Double -> Double -> Double
lose y0 p = y0 - (p * y0) 
  
-- wins = 1/4 chance of 10*x
wins :: Double -> Double -> Double
wins  y0 p = y0 - (p * y0) + (10 * (p * y0))
  
-- mean takes object*probability for each pair in our Dist
-- The sum of all of these is the mean of our Dist
mean :: Dist Double -> Double
mean (Dist xs) = (sum (map (\(x,p) -> x * p) xs))
  
-- Find Max value of p by plotting 
-- mean (dreidelDreidelDreidel 1000 p 10)

{-
	We know our expected return is 2.5*x. So if we bet all of our money our return should be (2.5*x) for each x (starting at 1000 and changing after each iteration).
	Check this value for some levels of bets by running mean (dreidelDreidelDreidel 1000 p 10) for p from 0.1 -> 1.
	
	p=0.0 = €1000.00
	p=0.1 = €4045.56
	p=0.2 = €12785.85
	p=0.3 = €41084.69
	p=0.4 = €109951.16
	p=0.5 = €269389.39
	p=0.6 = €613106.63
	p=0.7 = €1310806.57
	p=0.8 = €2655992.28
	p=0.9 = €5136634.01
	p=1.0 = €9536743.16
	When graphed in Excel, this was exponential.
	Basically p=1 maximizes mean (dreidelDreidelDreidel 1000 p 10) even though logic would dictate a 1 win to 1023 loss chance doesn't make sense, the huge winnings mess up the mean.
-}

-- Calculates probability of a sample meeting or exceeding target
prExceeds :: Double -> Dist Double -> Double
prExceeds target dist = mean $ fmap (fromIntegral . fromEnum . (>= target)) dist


{-
	Find Max value of p by plotting
	From last question it's that you'll win the expected value when you only have 1 chance in 1024 of winning anything at all. So we
	are looking for a value which gives us the best guarantee for a 4.x return of original investment after 10 trials. 
	prExceeds 4000 (dreidelDreidelDreidel 1000 p 10)
	p=0 -> 0
	p=0.05 -> 0.078
	p=0.10 -> 0.224
	p=0.15 -> 0.224
	p=0.2  -> 0.474
	p=0.25 -> 0.474
	p=0.3  -> 0.474
	p=0.35 -> 0.224
	p=0.4  -> 0.224
	p=0.45 -> 0.224
	p=0.5  -> 0.224
	p=0.55 -> 0.224
	p=0.6  -> 0.224
	p=0.65 -> 0.224
	p=0.7  -> 0.078
	p=0.75 -> 0.078
	p=0.8  -> 0.078
	p=0.85 -> 0.019
	p=0.9  -> 0.019
	p=0.95 -> 0.019
	p=1.0  -> 0.0000009
	
	What is p?
	It turns out we can use any p-value between ~0.14 and ~0.31
	You may as well take 0.31 to be p.
	
	What is the mean end pot?
    €45538.24
	
	What is the probability of ending up with over 4000?
	47.4% (0.474)

-}
