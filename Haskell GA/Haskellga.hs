module Haskellga
 where
import System.Random
import Control.Monad
import Control.Monad.IO.Class
--pi = 3.14159265359
a = -5.14
b = 5.14
l = ceiling (logBase 2 ((10^5)*(b-a)))
nrDim = 5
mutationProb = 0.97
crossoverProb = 0.3
popSize = 100
chromLength = nrDim * l
generations = 1000
--Random boolean generator https://stackoverflow.com/questions/43773538/how-to-create-a-random-boolean-generator-in-haskell
generateChrom :: Int ->  IO [Bool]
generateChrom size = replicateM size randomIO

--test :: Int -> IO Int
--test _ = do return 1

--From binary to decimal
bintodec :: [Bool] -> Int
bintodec [] = 0
bintodec (hd:tl) = (2^(length tl))*(fromEnum hd)+(bintodec tl)




--Replace the nth element of a list
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

--Generate a population of popSize chromosomes, each of length chromLength

generatePop :: Int -> IO [[Bool]]
generatePop size = replicateM size (generateChrom chromLength)


--Change a bit at a given position from a chromosome
flipBitAtPosition :: Int -> [Bool] -> [Bool]
flipBitAtPosition _ [] = []
flipBitAtPosition n (hd:tl) | n == 0 = (not hd):tl
                            | otherwise = hd:flipBitAtPosition (n-1) tl
--Generate a random number between 0 and 1
getProb :: IO Double
getProb = randomRIO(0.0,1.0) :: IO Double


--Compare a number with the mutation probability

compareDouble  :: Double -> Bool
compareDouble n | n < mutationProb = True
                | otherwise = False


--Mutate one gene from a chromosome
mutation :: [Bool] -> Int -> IO [Bool]
mutation chrom n = do
	if n < 0 
		then return chrom
		else
			do
			  mutateGene <- fmap (compareDouble) getProb
			  if mutateGene == True
			  	then return (flipBitAtPosition n chrom)
			  	else mutation chrom (n-1)

--Apply the mutation to all the chromosomes from a population
mutatePop :: [[Bool]] -> Int -> [IO [Bool]] -> [IO [Bool]]
mutatePop (hd:tl) n newPop | n==0 = newPop
                           | otherwise = mutatePop tl (n-1) (newPop ++ [(mutation hd chromLength)])
mutatePop [] _ newPop = newPop
--Convert from [IO [Bool]] to IO [[Bool]]
newPop :: [IO [Bool]] -> [[Bool]] -> IO [[Bool]]
newPop [] new = do
	return new
newPop (hd:tl) new = do
	chrom <- hd
	newPop tl (new ++ [chrom])
--Calculate Rastrigin (x0,x1...xn)
rastrigin :: Int -> [Double] -> Double
rastrigin n [] = fromIntegral n * 10
rastrigin n (hd:tl) = hd*hd - 10 * cos (2 * pi * hd) + rastrigin n tl
--Shift a value to a range between a and b
shift :: Double -> Double
shift value = (value / ((2^l)-1)) * (b-a) + a 

getL :: Int
getL = l
--Decode a chromosome and get a list of values for the dimensions (x0,x1...)
decodeChrom :: [Bool] -> Int -> [Bool] -> [Double]
decodeChrom [] _ x = [shift (fromIntegral (bintodec x))]
decodeChrom (hd:tl) l x | l==0 = shift (fromIntegral (bintodec x)) : (decodeChrom (hd:tl) (getL) ([]))
decodeChrom (hd:tl) l x | otherwise = decodeChrom tl (l-1) (x ++ [hd]) 
--Call decodeChrom with the right parameters and get the value for Rastrigin function, given the result of decodeChrome
decodeChromWrapper :: [Bool] -> Double
decodeChromWrapper chrom = rastrigin  (nrDim) (decodeChrom chrom l []) 
--Given a chromosome, calculate its fitness
getFitness :: [Bool] -> Double
getFitness chrom = 1 / (0.01 + decodeChromWrapper chrom)
--Get total fitness for a population
getTotalFitness :: [[Bool]] -> Double
getTotalFitness [] = 0
getTotalFitness (hd:tl) = getFitness (hd) + getTotalFitness tl
--Given a population and its total fitness,
--Build the roulette wheel for that population
--The formula is: wheel(cn) = wheel(cn-1)+fit(cn)/fit(pop)
getWheel :: [[Bool]] -> Double -> [Double] -> [Double]
getWheel [] _ wheel = wheel
getWheel (hd:tl) totalFitness [] = getWheel tl totalFitness [(getFitness hd)/totalFitness] 
getWheel (hd:tl) totalFitness wheel  = getWheel tl totalFitness ((wheel) ++ [last (wheel) + (getFitness hd)/totalFitness])

--Call getWheel with the right parameters
getWheelWrapper :: [[Bool]] -> Double -> [Double]
getWheelWrapper pop totalFitness = getWheel pop totalFitness []
--Given a place on the wheel, the wheel and the population,
--Select the chromosome corresponding to the place on the wheel
selectChromosome :: Double -> [Double] -> [[Bool]] -> [Bool]
selectChromosome p (hd:tl) (hd1:tl1) | p <= hd = hd1
                                    | otherwise = selectChromosome p tl tl1 

--Spin the roulette wheel popSize times
--Each time generate a random place on the wheel (number between 0 and the last value on the wheel)
--And add the corresponding chromosome to the new population
spinWheel :: [Double] -> [[Bool]] -> [[Bool]] -> Int ->  IO [[Bool]]
spinWheel wheel pop newPop rotationsRemaining = do
	if rotationsRemaining == 0
		then 
			return newPop
		else do
		 --print rotationsRemaining
		 p <- getProb
		 let placeOnWheel = p * (last wheel)
		 let chrom = selectChromosome placeOnWheel wheel pop
		 spinWheel wheel pop (newPop ++ [chrom]) (rotationsRemaining - 1)
	

spinWheelWrapper :: [Double] -> [[Bool]] -> IO [[Bool]]
spinWheelWrapper wheel pop = spinWheel wheel pop [] popSize

--Iterate throughout the population and decide whether the current
--Chromosome will be put in the crossover pool or not, based on
--The crossover probability
getCrossoverPool :: Int -> [Int] -> IO [Int]
getCrossoverPool 0 pool = do
	if length (pool) `mod` 2 == 0 then 
	    return pool
	else
		return (init pool)
getCrossoverPool n pool =
	do
		p <- getProb
		if p < crossoverProb then
			getCrossoverPool (n-1) (pool ++ [n])
			else
				getCrossoverPool (n-1) pool

getCrossoverPoolWrapper :: IO [Int]
getCrossoverPoolWrapper = getCrossoverPool popSize []
--Given two chromosomes and a split point, cross those two chromosomes
--The result is a pair consisting of the result of the crossover
crossChromosomes :: [Bool] -> [Bool] -> Int -> Int -> ([Bool],[Bool]) -> ([Bool],[Bool])
crossChromosomes _ _ position 0 pair = pair
crossChromosomes (hd1:tl1) (hd2:tl2) position currentPosition (chrom1,chrom2) | currentPosition > position = crossChromosomes (tl1) (tl2) position (currentPosition - 1) (chrom1 ++ [hd1],chrom2 ++ [hd2])
                                                                              | otherwise = crossChromosomes (tl1) (tl2) position (currentPosition - 1) (chrom1 ++ [hd2],chrom2 ++ [hd1])
crossChromosomesWrapper :: [Bool] -> [Bool] -> Int -> ([Bool],[Bool])
crossChromosomesWrapper chrom1 chrom2 splitPoint = crossChromosomes chrom1 chrom2 splitPoint chromLength ([],[])

--Get the nth chromosome from a population
getNthChromosome :: [[Bool]] -> Int -> [Bool]
getNthChromosome (hd:tl) pos | pos == 1 = hd
                             | otherwise = getNthChromosome (tl) (pos - 1)
--Iterate through the crossover pool and cross the chromosomes,
--2 by 2, the results are put in a new population, concatenated
--With a population containing the chromosomes which are not
--In the crossover pool
crossover :: [[Bool]] -> [Int] -> [[Bool]] -> IO [[Bool]]
crossover _ [] crossedPop = do
 return crossedPop
crossover pop (hd1:(hd2:tl)) crossedPop = do
	position <- randomRIO (1,chromLength) :: IO Int
	let chrom1 = getNthChromosome pop hd1
	let chrom2 = getNthChromosome pop hd2
	let crossedChromosomesPair = crossChromosomesWrapper chrom1 chrom2 position
	let newChrom1 = fst crossedChromosomesPair
	let newChrom2 = snd crossedChromosomesPair
	crossover pop (tl) (crossedPop ++ [newChrom1] ++ [newChrom2])
--Iterate through the population and add to a new population
--The chromosomes which were not selected for crossover
getRemainingChromosomes :: [[Bool]] -> [Int] -> Int -> [[Bool]] -> [[Bool]]
getRemainingChromosomes [] _ _ remaining = remaining
getRemainingChromosomes (hd:tl) [] n remaining = getRemainingChromosomes tl [] n (remaining ++ [hd])
getRemainingChromosomes (hd:tl) pool n remaining | n == last (pool) = getRemainingChromosomes tl (init pool) (n+1) remaining
                                                 | otherwise = getRemainingChromosomes tl (pool) (n+1) (remaining ++ [hd])


getRemainingChromosomesWrapper :: [[Bool]] -> [Int] -> [[Bool]]
getRemainingChromosomesWrapper pop pool = getRemainingChromosomes pop pool 1 []

crossoverWrapper :: [[Bool]] -> [Int] -> IO [[Bool]]
crossoverWrapper pop crossoverPool = do
	crossedChromosomes <- crossover pop crossoverPool []
	let uncrossedChromosomes = getRemainingChromosomesWrapper pop crossoverPool
	return (crossedChromosomes ++ uncrossedChromosomes)
--Get the best chromosome from a population
minimumValueFromPop :: [[Bool]] -> Double -> [Double] -> (Double,[Double])
minimumValueFromPop [] currentValue currentChrom = (currentValue, currentChrom)
minimumValueFromPop (hd:tl) currentValue currentChrom | currentValue <= decodeChromWrapper hd = minimumValueFromPop tl currentValue currentChrom
                                         | otherwise = minimumValueFromPop tl (decodeChromWrapper hd) (decodeChrom hd l []) 

genetic :: [[Bool]] -> Int -> (Double, [Double])  -> IO (Double, [Double])
genetic _ 0 result = do
	return result
genetic pop iterations result = do
	--Mutate the current population and convert from [IO [Bool]] to IO [[Bool]]
 let mutatedPop = mutatePop pop popSize []
 newPopulation <- newPop mutatedPop []
--Calculate the population's fitness and build the roulette wheel
 let popFitness = getTotalFitness newPopulation
 let rouletteWheel = getWheelWrapper newPopulation popFitness
 --Spin the wheel and get a new population
 populationAfterSelection <- spinWheelWrapper rouletteWheel newPopulation
 --Build the crossover pool and apply the crossover operator
 crossoverPool <- getCrossoverPoolWrapper
 populationAfterCrossover <- crossoverWrapper populationAfterSelection crossoverPool
 --Compare the current minimum with the global minimum
 let minim = minimumValueFromPop populationAfterCrossover (10000000000) []
 if fst result > fst minim then
  genetic populationAfterCrossover (iterations - 1) minim
  else
  	genetic populationAfterCrossover (iterations - 1) result

andreisGeneticAlgorithm ::  IO (Double, [Double])
andreisGeneticAlgorithm = do
	pop <- generatePop popSize
	result <- genetic pop generations (10000000000, [])
	return result