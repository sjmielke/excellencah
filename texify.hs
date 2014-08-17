import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*), (*>))
import Control.Arrow (first)
import Data.List (transpose, sort, group)
import qualified Data.Text as T (pack, count)

type Question = String
type Answer = String

onlySingleQuestions :: [Question] -> [Question]
onlySingleQuestions = filter ((==1) . T.count (T.pack "_____") . T.pack)

parseCAHCSV :: String -> ([Question], [Answer])
parseCAHCSV = either (error . show) (interpretData
                                    . map (filter $ not . null)
                                    . transpose)
            . parse csvParser "Raw question/answer CSV"
    where interpretData [answers, questions] = (questions, answers)
          interpretData _ = error "CSV seems to have contained not exactly two columns!"

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
          print $ parseCAHCSV csv
          print $ first onlySingleQuestions $ parseCAHCSV csv
