{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Parsec.Adapter
  -- Text.Parsec.Prim
  ( Parser
  , (<?>)
  , (<|>)
  , lookAhead
  , try
  , many
  , skipMany
  , runParserT
  , getPosition
  , getInput
  , setInput
  , getState
  , updateState
  -- Text.Parsec.Combinator
  , choice
  , many1
  , manyTill
  , skipMany1
  , option
  , optionMaybe
  , anyToken
  , notFollowedBy
  , between
  , eof
  -- Text.Parsec.Char
  , oneOf
  , space
  , upper
  , lower
  , alphaNum
  , letter
  , digit
  , hexDigit
  , octDigit
  , char
  , anyChar
  , satisfy
  , string
  -- Text.Parsec.Pos
  , SourceName
  , SourcePos
  , newPos
  , sourceLine
  , sourceColumn
  -- Text.Parsec.Error
  , Message
  , ParseError
  , newErrorUnknown
  , errorPos
  , errorMessages
  -- Text.Parsec.Indent
  , IndentParser
  , runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.State as State
import qualified Control.Monad.Fail as Fail

import qualified Parse.Primitives as EP
import Parse.State (State)

import Data.List (nub, sort)
import Data.Typeable (Typeable)



-- Text.Parsec.Prim


unknownError :: EP.Row -> EP.Col -> ParseError
unknownError row col =
  newErrorUnknown $ newPos "" row col


data Parser a
  = Parser (EP.Parser ParseError a)


instance Functor Parser where
    fmap f (Parser p) = Parser (fmap f p)


instance Applicative.Applicative Parser where
    pure = return
    (Parser f) <*> (Parser a) = Parser (f <*> a)

instance Applicative.Alternative Parser where
    empty = mzero
    (<|>) = mplus


instance Monad Parser where
    return = Parser . return
    (Parser p) >>= f = Parser $ p >>= unwrap . f

unwrap :: Parser a -> EP.Parser ParseError a
unwrap (Parser p) = p


instance Fail.MonadFail Parser where
    fail = undefined


instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus


parserZero :: Parser a
parserZero =
  Parser $ EP.Parser $ \state _ _ _ eerr ->
    let
      (EP.State _ _ _ _ row col) = state
    in
    eerr row col unknownError


parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (Parser p) (Parser q) =
  Parser $ EP.oneOf (\row col -> undefined) [p, q]


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus


(<?>) :: Parser a -> String -> Parser a
(<?>) = undefined


lookAhead :: Parser a -> Parser a
lookAhead = undefined


try :: Parser a -> Parser a
try (Parser (EP.Parser parser)) =
  Parser $ EP.Parser $ \s cok eok _ err ->
    parser s cok eok err err

many :: Parser a -> Parser [a]
many = undefined

skipMany ::Parser a -> Parser ()
skipMany = undefined

runParserT :: Parser a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT = undefined

getPosition :: Parser SourcePos
getPosition = undefined

getInput :: Parser s
getInput = undefined

setInput :: s -> Parser ()
setInput = undefined

getState :: Parser State
getState = undefined

updateState :: (State -> State) -> Parser ()
updateState = undefined



-- Text.Parsec.Combinator


choice :: [Parser a] -> Parser a
choice = undefined

many1 :: Parser a -> Parser [a]
many1 = undefined

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill = undefined

skipMany1 :: Parser a -> Parser ()
skipMany1 = undefined

option :: a -> Parser a -> Parser a
option = undefined

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = undefined

anyToken :: Parser Char
anyToken = undefined

notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy = undefined

between :: Parser open -> Parser close -> Parser a -> Parser a
between = undefined

eof :: Parser ()
eof = undefined



-- Text.Parsec.Char


oneOf :: [Char] -> Parser Char
oneOf = undefined

space :: Parser Char
space = undefined

upper :: Parser Char
upper = undefined

lower :: Parser Char
lower = undefined

alphaNum :: Parser Char
alphaNum = undefined

letter :: Parser Char
letter = undefined

digit :: Parser Char
digit = undefined

hexDigit :: Parser Char
hexDigit = undefined

octDigit :: Parser Char
octDigit = undefined

char :: Char -> Parser Char
char = undefined

anyChar :: Parser Char
anyChar             = satisfy (const True)

satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

string :: String -> Parser String
string = undefined



-- Text.Parsec.Pos


type SourceName = String


data SourcePos = SourcePos SourceName !EP.Row !EP.Col


newPos :: String -> EP.Row -> EP.Col -> SourcePos
newPos =
  SourcePos


sourceLine :: SourcePos -> Int
sourceLine (SourcePos _ row _) =
  fromIntegral row


sourceColumn :: SourcePos -> Int
sourceColumn (SourcePos _ _ col) =
  fromIntegral col


instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"



-- Text.Parsec.Error


data Message
  = SysUnExpect !String -- @ library generated unexpect
  | UnExpect    !String -- @ unexpected something
  | Expect      !String -- @ expecting something
  | Message     !String -- @ raw message
  deriving ( Typeable )


instance Enum Message where
    fromEnum (SysUnExpect _) = 0
    fromEnum (UnExpect    _) = 1
    fromEnum (Expect      _) = 2
    fromEnum (Message     _) = 3
    toEnum _ = error "toEnum is undefined for Message"


instance Eq Message where
    m1 == m2 = fromEnum m1 == fromEnum m2


instance Ord Message where
    compare msg1 msg2 = compare (fromEnum msg1) (fromEnum msg2)


messageString :: Message -> String
messageString (SysUnExpect s) = s
messageString (UnExpect    s) = s
messageString (Expect      s) = s
messageString (Message     s) = s


data ParseError = ParseError !SourcePos [Message]


newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos
    = ParseError pos []


errorPos :: ParseError -> SourcePos
errorPos = undefined

errorMessages :: ParseError -> [Message]
errorMessages = undefined


instance Show ParseError where
    show err
        = show (errorPos err) ++ ":" ++
          showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                           (errorMessages err)


showErrorMessages ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
      (expect,messages)   = span ((Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany pre msgs3 = case clean (map messageString msgs3) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = separate ", " . clean

      separate   _ []     = ""
      separate   _ [m]    = m
      separate sep (m:ms) = m ++ sep ++ separate sep ms

      clean             = nub . filter (not . null)



-- Text.Parsec.Indent


type IndentParser a = Parser a

runIndent :: SourceName -> State.State SourcePos a -> a
runIndent = undefined

block :: IndentParser a -> IndentParser [a]
block = undefined

indented :: IndentParser ()
indented = undefined

checkIndent :: IndentParser ()
checkIndent = undefined

withPos :: IndentParser a -> IndentParser a
withPos = undefined
