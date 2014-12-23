module Main where
import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
       
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"
       
spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             deriving (Show)
     

parseChar :: Parser LispVal
parseChar = do try $ string "#\\"
               y <- many (noneOf " )")
               return . Char $ case y of
                                    "space" -> ' '
                                    "newline" -> '\n'
                                    otherwise -> head y
parseString :: Parser LispVal
parseString = do try $ char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
            
escapedCharacters :: Parser Char
escapedCharacters = do try $ char '\\' 
                       x <- oneOf "\\\"rnt"
                       return $ case x of
                                     'r' -> '\r'
                                     'n' -> '\n'
                                     't' -> '\t'
                                     otherwise -> x

-- String should allow \' inside it
parseStringR5RS :: Parser LispVal
parseStringR5RS = do try $ char '"'
                     --x <- many (noneOf "\"" <|> escapedCharacters)
                     x <- many (escapedCharacters  <|> noneOf "\"")
                     char '"'
                     return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom
          
-- parseNumber = do x <- many1 digit
--                 return $ (Number . read) x
--parseNumber = many1 digit >>= \x -> return $ (Number . read) x
--parseNumber = liftM (Number . read) $ many1 digit

parseNumber :: Parser LispVal
parseNumber = try $ liftM (Number . read) $ many1 digit
            
parseHex :: Parser LispVal
parseHex = try (string "#x") >> many1 hexDigit
           >>= return . Number . fst . head . readHex

parseOct :: Parser LispVal
parseOct = try (string "#o") >> many1 octDigit
           >>= return . Number . fst . head . readOct
         
readBin :: Integer -> String -> Integer
readBin accum [] = accum
readBin accum (x:xs) = readBin (accum * 2 + (read [x])) xs

parseBin :: Parser LispVal
parseBin = try $ (string "#b") >> many1 (oneOf "01")
              >>= return . Number . readBin 0

parseDec :: Parser LispVal
parseDec = try $ string "#d" >> many1 digit
               >>= return . Number . read
--parseNumber = liftM (Number . read) $ many1 digit
         
parseNumbers :: Parser LispVal
parseNumbers = parseNumber <|> parseDec <|> parseHex <|> parseOct <|> parseBin

parseBool :: Parser LispVal
parseBool = do try $ char '#'
               y <- try $ oneOf "tf"
               return $ Bool (case y of
                                       't' -> True
                                       otherwise -> False)
parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseStringR5RS
          <|> parseNumbers
          <|> try parseBool
          <|> parseChar
          <|> parseQuoted
          <|> do char '('
                 x <- do (try parseList <|> parseDottedList)
                 char ')'
                 return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
         Left err -> "No match: " ++ show err
         Right val -> "Found value: " ++ show val
         

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces
          
parseDottedList :: Parser LispVal
parseDottedList = do
          head <- endBy parseExpr spaces
          tail <- char '.' >> spaces >> parseExpr
          return $ DottedList head tail
          

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [ Atom "Quote", x ]

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
