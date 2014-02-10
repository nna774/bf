import Text.Parsec hiding(State)
import Text.Parsec.String
import Text.Parsec.Prim as Prim hiding(State)
import Control.Monad.State
import Control.Arrow hiding(loop)
import Control.Applicative hiding((<|>),many)
import Data.Word
import Data.Char

data BF = Inc | Dec | Lt | Gt | Out | Loop [BF] deriving (Eq)

instance Show BF where
    show Inc = "+"
    show Dec = "-"
    show Lt = "<"
    show Gt = ">"
    show Out = "."
    show (Loop bs) = '[' : show bs ++ "]"

bf :: Parser BF
bf = Prim.try inc <|> Prim.try dec <|> Prim.try lt <|> Prim.try gt <|> Prim.try out <|> Prim.try loop 

inc :: Parser BF
inc = char '+' >> return Inc

dec :: Parser BF
dec = char '-' >> return Dec

lt :: Parser BF
lt = char '<' >> return Lt

gt :: Parser BF
gt = char '>' >> return Gt

out :: Parser BF
out = char '.' >> return Out

loop :: Parser BF
loop = do
  char '['
  ins <- many bf
  char ']'
  return $ Loop ins

makeTree :: String -> [BF]
makeTree str = case parse (many bf) "BF" str of
                 Left err  -> error $ show err
                 Right val -> val

type Memory = ([Word8],[Word8]) -- fst is [ptr..0]; snd is [ptr+1..]
type Output = String

type World = (Memory, Output)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

biC :: (Word8 -> Word8) -> State World ()
biC f = modify (first . first $ mapHead f)

nextWorld :: BF -> State World ()
nextWorld Inc = biC (+1)
nextWorld Dec = biC (subtract 1)
nextWorld Lt = modify $ first f
  where
    f ([],_) = error "too many < (negative PC)"
    f ((x:xs), ys) = (xs, x:ys)
nextWorld Gt = modify $ first f
  where
    f (xs,[]) = (0:xs,[])
    f (xs, (y:ys)) = (y:xs, ys)
nextWorld Out = modify $ \(mem,os) -> let o = chr $ fromIntegral $ head $ fst mem in (mem,o:os)
nextWorld l@(Loop bs) = do
  (((flg:_),_),_) <- get
  when (flg /= 0) $ (evalBFCode bs >> nextWorld l)

evalBFCode :: [BF] -> State World ()
evalBFCode = sequence_ . map nextWorld 

whiteWorld :: World
whiteWorld = (([0],[]),"")

eval :: [BF] -> String
eval code = reverse $ snd $ execState (evalBFCode code) whiteWorld

unrollImpHelper :: BF -> State World [BF]
unrollImpHelper b = nextWorld b >> return [b]

unrollImp :: BF -> State World [BF]
unrollImp l@(Loop bs) = do
  (((flg:_),_),_) <- get
  if (flg == 0)
  then return []
  else (++) <$> (unrollBFCode bs) <*> (unrollImp l)

unrollImp b = unrollImpHelper b

unrollBFCode :: [BF] -> State World [BF]
unrollBFCode = fmap concat . sequence . map unrollImp 

unroll :: [BF] -> [BF]
unroll code = evalState (unrollBFCode code) whiteWorld

runBF :: String -> String
runBF = eval . makeTree

runUnroll :: String -> String
runUnroll = bfShow . unroll . makeTree

bfShow :: [BF] -> String
bfShow = concat . map show

main :: IO ()
main = getContents >>= return . runBF  >>= putStrLn

