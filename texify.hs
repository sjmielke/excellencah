import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*), (*>))
import Control.Arrow (first)
import Control.Monad (when)
import Data.List (transpose, sort, group, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text as T (pack, count)
import System.Environment (getArgs)
import System.Process (system)

type Question = String
type Answer = String

onlySingleQuestions :: [Question] -> [Question]
onlySingleQuestions = filter ((==1) . T.count (T.pack "_____") . T.pack)

getListAsTex :: [String] -> String
getListAsTex = concat . map (\s -> "\\cahcard{" ++ latexrep s ++ "}%\n")
    where latexrep = replace "_____" "\\underline{\\hspace{1cm}}"
                   . replace "%" "\\%"
                   . replace "&" "\\&"
                   . replace "$" "\\$"
                   . replace "#" "\\#"
                   . replace "€" " Euro"
                   . replace "\\" "\\textbackslash{}"
          replace old new = intercalate new . splitOn old

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

main = do args <- getArgs
          when (length args /= 1) $ error "Please pass one parameter with the filename of the CSV containing questions and answers."
          csv <- readFile $ head args
          let (questions, answers) = parseCAHCSV csv
          writeFile "latex/questions.tex" $ getListAsTex . onlySingleQuestions $ questions
          writeFile "latex/answers.tex" $ getListAsTex $ answers
          putStrLn "Now pdflatex has to be called:"
          putStrLn "cd latex && pdflatex cards.tex"
          putStrLn "This program will try to do this itself... which might fail on your system."
          system "cd latex && pdflatex cards.tex"
