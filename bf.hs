import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim as Prim

data BF = Inc | Dec | Lt | Gt | OUT | Loop [BF] deriving (Eq, Show, Read)

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
out = char '.' >> return OUT

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

main :: IO ()
main = getContents >>= return . show . makeTree >>= putStrLn
