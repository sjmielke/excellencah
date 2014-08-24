import Text.Parsec
import Text.Parsec.String
import Text.Read (readMaybe)
import Control.Applicative ((<*), (*>))
import Control.Arrow (first)
import Control.Monad (when, liftM, liftM2)
import Data.List (transpose, sort, group, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text as T (pack, count)
import System.Console.ArgParser
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

data CLIFlags = CSVCards { csvFilename :: FilePath
                         , noOfUnderscores :: Int
                         }
              | TXTCards { questionsFilename :: FilePath
                         , answersFilename :: FilePath
                         , noOfUnderscores :: Int
                         }
              deriving Show

main = mkSubParser [("csv", csv), ("txt", txt)] >>= flip runApp runWithFlags
    where
        csv = mkDefaultApp (CSVCards `parsedBy` reqPos "csvFilename" `Descr` "File name of the CSV containing questions and answers in two columns"
                                     `andBy` reqPos "noOfUnderscores" `Descr` "Number of underscores denoting a blank"
                           )
                           "csv"
              `setAppDescr` "Parse from a CSV containing questions and answers in two columns"
        txt = mkDefaultApp (TXTCards `parsedBy` reqPos "questionsFilename" `Descr` "File name of the text file containing questions"
                                     `andBy` reqPos "answersFilename" `Descr` "File name of the text file containing answers"
                                     `andBy` reqPos "noOfUnderscores" `Descr` "Number of underscores denoting a blank"
                           )
                           "txt"
              `setAppDescr` "Parse from two TXT files containing questions and answers"
        runWithFlags myFlags = do
            (questions, answers) <- case myFlags of
                                        (CSVCards _ _)   -> liftM parseCAHCSV
                                                            (readFile $ csvFilename myFlags)
                                        (TXTCards _ _ _) -> liftM2 (,)
                                                            (liftM (filter (not . null) $ lines) $ readFile $ questionsFilename myFlags)
                                                            (liftM (filter (not . null) $ lines) $ readFile $ answersFilename myFlags)
            let nou = noOfUnderscores myFlags
            writeFile "latex/questions.tex" $ getListAsTex nou . onlySingleQuestions nou $ questions
            writeFile "latex/answers.tex" $ getListAsTex nou $ answers
            putStrLn "Now pdflatex has to be called:"
            putStrLn "cd latex && pdflatex cards.tex"
            putStrLn "This program will try to do this itself... which might fail on your system."
            system "cd latex && pdflatex cards.tex"
            return ()
