import Text.Parsec
import Text.Parsec.String
import Text.Read (readMaybe)
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

onlySingleQuestions :: Int -> [Question] -> [Question]
onlySingleQuestions noOfUnderscores = filter ( (==1)
                                             . T.count (T.pack $ replicate noOfUnderscores '_')
                                             . T.pack)

getListAsTex :: Int -> [String] -> String
getListAsTex noOfUnderscores = concat . map (\s -> "\\cahcard{" ++ latexrep s ++ "}%\n")
    where latexrep = replace (replicate noOfUnderscores '_') "\\underline{\\hspace{1cm}}"
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
          when (length args /= 2) $ error "Please pass one parameter with the filename of the CSV containing questions and answers and one with the number of underscores that will denote a blank."
          let [csvFilename, noOfUnderscoresParam] = args
              noOfUnderscores = case (readMaybe noOfUnderscoresParam) :: Maybe Int of
                                    Just n  -> n
                                    Nothing -> error "The second parameter (number of underscores that denote a blank) was not a number!"
          csv <- readFile csvFilename
          let (questions, answers) = parseCAHCSV csv
          writeFile "latex/questions.tex" $ getListAsTex noOfUnderscores . onlySingleQuestions noOfUnderscores $ questions
          writeFile "latex/answers.tex" $ getListAsTex noOfUnderscores $ answers
          putStrLn "Now pdflatex has to be called:"
          putStrLn "cd latex && pdflatex cards.tex"
          putStrLn "This program will try to do this itself... which might fail on your system."
          system "cd latex && pdflatex cards.tex"
