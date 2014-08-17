import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*), (*>))

csvParser :: GenParser Char () [[String]]
csvParser = line `sepBy` eol
    where
        line = (try quotedCell <|> simpleCell) `sepBy` (char ',')
        simpleCell = many $ noneOf ",\r\n"
        quotedCell = char '"' *> (many quotedChar) <* char '"'
        quotedChar = (try (string "\"\"") *> return '"') <|> (noneOf "\"")
        eol =     try (string "\n")
              <|> try (string "\r")
              <|> try (string "\r\n")
              <|> try (string "\n\r")

main = do csv <- readFile "ex.csv"
          print $ mapRight (filter $ not . null . head) $ parse csvParser "Example" csv
    where mapRight f x@(Left _) = x
          mapRight f x@(Right a) = (Right $ f a)
