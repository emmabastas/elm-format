{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Parsec.Adapter
  ( Parser
  -- Text.Parsec.Prim
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
  , runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.State as Monad
import qualified Control.Monad.Fail as Fail

import qualified Parse.Primitives as EP
import Parse.State (State(..))

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Indent as P

import Data.List (nub, sort)
import Data.Typeable (Typeable)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign.Ptr (plusPtr, minusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Codec.Binary.UTF8.String (encode, decode)


data Parser a
  = Parser (EP.Parser ParseError a)


type ParsecInput = String
type ParsecParser a = P.ParsecT ParsecInput State (Monad.State SourcePos) a


parserSec :: ParsecParser a -> Parser a
parserSec p =
  Parser $ EP.Parser $ \s cok eok cerr eerr ->
    let
      row = fromIntegral $ EP._row s
      col = fromIntegral $ EP._col s
      sourcePos = newPos "" row col
    in
    case Monad.evalState (P.runParsecT p (stateRet s)) sourcePos of
      P.Consumed x ->
        case Monad.evalState x sourcePos of
          P.Ok output state _ ->
            cok output (stateSec state)

          P.Error parseError ->
            let
              sourcePos = P.errorPos parseError
              row = fromIntegral $ P.sourceLine sourcePos
              col = fromIntegral $ P.sourceColumn sourcePos
            in
            cerr row col (\_ _ -> parseError)

      P.Empty x ->
        case Monad.evalState x sourcePos of
          P.Ok output state _ ->
            eok output (stateSec state)

          P.Error parseError ->
            let
              sourcePos = P.errorPos parseError
              row = fromIntegral $ P.sourceLine sourcePos
              col = fromIntegral $ P.sourceColumn sourcePos
            in
            eerr row col (\_ _ -> parseError)


parserRet :: Parser a -> ParsecParser a
parserRet (Parser (EP.Parser p )) =
  P.mkPT $ \state ->
    let
      cok v s =
        let s' = stateRet s in
        return $ P.Consumed $ return $ P.Ok v s' (unknownError s')

      eok v s =
        let s' = stateRet s in
        return $ P.Empty $ return $ P.Ok v s' (unknownError s')

      cerr row col toError =
        return $ P.Consumed $ return $ P.Error $ toError row col

      eerr row col toError =
        return $ P.Empty $ return $ P.Error $ toError row col
    in
    p (stateSec state) cok eok cerr eerr


stateSec :: P.State ParsecInput State -> EP.State
stateSec (P.State source sourcePos (State newline)) =
  let
    (B.PS fptr offset length) = B.pack $ encode source
    pos = plusPtr (unsafeForeignPtrToPtr fptr) offset
    end = plusPtr pos length
    row = fromIntegral $ P.sourceLine sourcePos
    col = fromIntegral $ P.sourceColumn sourcePos
  in
  EP.State fptr pos end 0 row col


stateRet :: EP.State -> P.State ParsecInput State
stateRet (EP.State fptr pos end indent row col) =
  let
    offset = minusPtr (unsafeForeignPtrToPtr fptr) pos
    length = minusPtr end pos
    source = decode $ B.unpack (B.PS fptr offset length)
    sourcePos = P.newPos "" (fromIntegral row) (fromIntegral col)
    userState = State []
  in
  P.State source sourcePos userState



-- Text.Parsec.Prim


unknownError :: P.State s u -> ParseError
unknownError = P.unknownError


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
  mzero = undefined
  mplus = undefined


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus


(<?>) :: Parser a -> String -> Parser a
(<?>) p s = parserSec $ (flip P.label) s $ parserRet p


lookAhead :: Parser a -> Parser a
lookAhead = parserSec . P.lookAhead . parserRet


try :: Parser a -> Parser a
try =
  parserSec . P.try . parserRet

many :: Parser a -> Parser [a]
many =
  parserSec . P.many . parserRet


skipMany ::Parser a -> Parser ()
skipMany = undefined

runParserT :: Parser a -> State -> SourceName -> ParsecInput -> Monad.State SourcePos (Either ParseError a)
runParserT parser userState sourceName state =
  P.runParserT (parserRet parser) userState sourceName state


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
option a p =
  parserSec $ P.option a $ parserRet p

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



type SourceName = P.SourceName


type Line = P.Line


type Column = P.Column


type SourcePos = P.SourcePos


newPos :: String -> Line -> Column -> SourcePos
newPos = P.newPos


sourceLine :: SourcePos -> Line
sourceLine = P.sourceLine


sourceColumn :: SourcePos -> Column
sourceColumn = P.sourceColumn




-- Text.Parsec.Error



type ParseError = P.ParseError


type Message = P.Message


newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown = P.newErrorUnknown


errorPos :: ParseError -> SourcePos
errorPos = P.errorPos

errorMessages :: ParseError -> [Message]
errorMessages = P.errorMessages


showErrorMessages ::
  String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages = P.showErrorMessages



-- Text.Parsec.Indent


runIndent :: SourceName -> Monad.State SourcePos a -> a
runIndent = P.runIndent

block :: Parser a -> Parser [a]
block = undefined

indented :: Parser ()
indented = undefined

checkIndent :: Parser ()
checkIndent = undefined

withPos :: Parser a -> Parser a
withPos = undefined
