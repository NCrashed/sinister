{-# LANGUAGE GADTs, DeriveGeneric #-}
module Assets.ObjMesh.Parser(
    parseObj
  , isObject
  , ObjAST(..)
  ) where 

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef, haskellDef)
import Data.Char
import Control.Monad 
import GHC.Generics (Generic)
import Control.DeepSeq

data ObjAST where
  ObjObject   :: String -> ObjAST
  ObjVertex   :: Double -> Double -> Double -> ObjAST
  ObjNormal   :: Double -> Double -> Double -> ObjAST
  ObjUv       :: Double -> Double -> ObjAST
  ObjSmooth   :: Bool -> ObjAST
  ObjLine     :: [(Int, Maybe Int)] -> ObjAST 
  ObjFace     :: [(Int, Maybe Int, Maybe Int)] -> ObjAST
  ObjMaterial :: String -> ObjAST 
  ObjMaterialLib :: String -> ObjAST
  deriving Generic 

instance NFData ObjAST

isObject :: ObjAST -> Bool
isObject (ObjObject _) = True
isObject _ = False
  
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    names = ["o", "v", "vt", "vn", "s", "off", "on", "f", "/", "usemtl", "mtllib", "l"]
    style = emptyDef {
      Tok.commentLine = "#",
      Tok.reservedNames = names
    }
    
integer :: Parser Int
integer = fromInteger <$> Tok.integer lexer

real :: Parser Double
real = Tok.lexeme lexer real' <?> "real" 
  where 
    real' = do
      s <- option id sign
      n <- option 0 $ Tok.decimal lexer
      _ <- char '.'
      frac <- option 0 fracPart
      exp' <- expPart
      return $ s (exp' (fromIntegral n + frac))
    expPart :: Parser (Double -> Double)
    expPart = option (*1.0) $ do
      _ <- oneOf "eE"
      es <- option id sign
      emantis <- Tok.decimal lexer
      return (* (10.0 ^^ es emantis))
    fracPart :: Parser Double
    fracPart = do
      ds <- many1 digit <?> "fraction"
      return (foldr op 0.0 ds)
    op d f = (f + fromIntegral (digitToInt d))/10.0
    
sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (char '+' >> return id)

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = qualifiedIdentifier <|> try (Tok.identifier lexer) <|> Tok.stringLiteral lexer
  where 
    qualifiedIdentifier = Tok.lexeme lexer $ try $ many1 $ Tok.identLetter haskellDef <|> char '.'
    --fmap concat $ try (Tok.identifier lexer) `sepBy1` Tok.symbol lexer "."

objParser :: Parser [ObjAST]
objParser = many $ choice [objObject, objVertex, objNormal, objUv, objSmooth, objLine, objFace, objMaterial, objMaterialLib]
  where
  objObject   = reserved "o" >> ObjObject <$> identifier
  objVertex   = reserved "v" >> ObjVertex <$> real <*> real <*> real
  objNormal   = reserved "vn" >> ObjNormal <$> real <*> real <*> real
  objUv       = reserved "vt" >> ObjUv <$> real <*> real
  objSmooth   = reserved "s" >> ObjSmooth <$> (onWord <|> offWorld)
  objFace     = reserved "f" >> ObjFace <$> many face
  objLine     = reserved "l" >> ObjLine <$> many line
  objMaterial = reserved "usemtl" >> ObjMaterial <$> identifier
  objMaterialLib = reserved "mtllib" >> ObjMaterialLib <$> identifier

  ch' = void . Tok.lexeme lexer . char
  onWord = (reserved "on" <|> ch' '1') >> return True
  offWorld = (reserved "off" <|> ch' '0') >> return False

  line :: Parser (Int, Maybe Int)
  line = do 
    vi <- integer
    uvi <- optionMaybe $ char '/' >> integer 
    return (vi, uvi)

  face :: Parser (Int, Maybe Int, Maybe Int)
  face = do
    vi <- integer
    _ <- char '/'
    uvi <- optionMaybe integer
    _ <- char '/'
    ni <- optionMaybe integer
    return (vi, uvi, ni) 
    
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
  
parseObj :: String -> String -> Either ParseError [ObjAST]
parseObj path = parse (contents objParser) path 