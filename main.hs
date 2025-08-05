{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (find)
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Combinator as P

-- Abstract Syntax Tree
data Term = 
    Entity Text
  | Variable Text
  deriving (Show, Eq, Ord)

data Relation = 
    IsA Term Term           -- "X ist ein Y"
  | Has Term Term           -- "X hat Y"
  | Loves Term Term         -- "X liebt Y"
  | Lives Term Term         -- "X lebt in Y"
  | WorksAt Term Term       -- "X arbeitet bei Y"
  deriving (Show, Eq)

data Statement = 
    Fact Relation
  | Rule [Relation] Relation  -- if conditions then conclusion
  deriving (Show, Eq)

data Query = 
    QueryRelation Relation
  | QueryExists Term
  deriving (Show, Eq)

-- Knowledge Base
type KnowledgeBase = [Statement]

-- Lexer helpers
spaces1 :: Parser ()
spaces1 = skipMany1 space

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- German articles and determiners
article :: Parser Text
article = choice $ map (\s -> T.pack <$> try (string s)) 
  ["der", "die", "das", "ein", "eine", "einem", "einer", "den", "dem"]

-- Basic German words for our controlled language
noun :: Parser Text
noun = choice $ map (\s -> T.pack <$> try (string s))
  ["Mann", "Frau", "Hund", "Katze", "Haus", "Stadt", "Unternehmen", 
   "Person", "Tier", "Gebäude", "Programmierer", "Arzt"]

properNoun :: Parser Text
properNoun = choice $ map (\s -> T.pack <$> try (string s))
  ["Hans", "Maria", "Berlin", "München", "Google", "BMW", "Rex", "Fluffy"]

verb :: Parser Text  
verb = choice $ map (\s -> T.pack <$> try (string s))
  ["ist", "hat", "liebt", "lebt", "arbeitet"]

preposition :: Parser Text
preposition = choice $ map (\s -> T.pack <$> try (string s))
  ["in", "bei", "von", "zu", "mit"]

-- Parse terms
parseTerm :: Parser Term
parseTerm = choice
  [ Entity <$> properNoun
  , Entity <$> noun
  , Variable <$> (T.pack <$> (string "X" <|> string "Y" <|> string "Z"))
  ]

-- Parse relations
parseIsA :: Parser Relation
parseIsA = do
  subj <- parseTerm
  spaces1
  _ <- string "ist"
  spaces1
  P.optional article
  spaces
  obj <- parseTerm
  return $ IsA subj obj

parseHas :: Parser Relation
parseHas = do
  subj <- parseTerm
  spaces1
  _ <- string "hat"
  spaces1
  P.optional article
  spaces
  obj <- parseTerm
  return $ Has subj obj

parseLoves :: Parser Relation
parseLoves = do
  subj <- parseTerm
  spaces1
  _ <- string "liebt"
  spaces1
  obj <- parseTerm
  return $ Loves subj obj

parseLives :: Parser Relation
parseLives = do
  subj <- parseTerm
  spaces1
  _ <- string "lebt"
  spaces1
  _ <- string "in"
  spaces1
  obj <- parseTerm
  return $ Lives subj obj

parseWorksAt :: Parser Relation
parseWorksAt = do
  subj <- parseTerm
  spaces1
  _ <- string "arbeitet"
  spaces1
  _ <- string "bei"
  spaces1
  obj <- parseTerm
  return $ WorksAt subj obj

parseRelation :: Parser Relation
parseRelation = choice
  [ try parseIsA
  , try parseHas
  , try parseLoves  
  , try parseLives
  , try parseWorksAt
  ]

-- Parse statements
parseFact :: Parser Statement
parseFact = Fact <$> parseRelation <* string "."

parseRule :: Parser Statement
parseRule = do
  _ <- string "Wenn"
  spaces1
  conditions <- parseRelation `sepBy1` (spaces1 >> string "und" >> spaces1)
  spaces1
  _ <- string "dann"
  spaces1
  conclusion <- parseRelation
  _ <- string "."
  return $ Rule conditions conclusion

parseStatement :: Parser Statement
parseStatement = choice [try parseRule, parseFact]

-- Parse queries
parseQueryRelation :: Parser Query
parseQueryRelation = do
  _ <- string "Ist es wahr dass"
  spaces1
  rel <- parseRelation
  _ <- string "?"
  return $ QueryRelation rel

parseQueryExists :: Parser Query
parseQueryExists = do
  _ <- string "Existiert"
  spaces1
  term <- parseTerm
  _ <- string "?"
  return $ QueryExists term

parseQuery :: Parser Query
parseQuery = choice [try parseQueryRelation, parseQueryExists]

-- Evaluation Engine
type Bindings = Map Text Term

-- Unification
unify :: Term -> Term -> Maybe Bindings
unify (Variable x) t = Just $ M.singleton x t
unify t (Variable x) = Just $ M.singleton x t
unify (Entity a) (Entity b) 
  | a == b = Just M.empty
  | otherwise = Nothing

-- Apply bindings to a term
applyBindings :: Bindings -> Term -> Term
applyBindings bindings (Variable x) = 
  case M.lookup x bindings of
    Just term -> term
    Nothing -> Variable x
applyBindings _ term = term

-- Apply bindings to a relation
applyBindingsRel :: Bindings -> Relation -> Relation
applyBindingsRel bindings rel = case rel of
  IsA t1 t2 -> IsA (applyBindings bindings t1) (applyBindings bindings t2)
  Has t1 t2 -> Has (applyBindings bindings t1) (applyBindings bindings t2)
  Loves t1 t2 -> Loves (applyBindings bindings t1) (applyBindings bindings t2)
  Lives t1 t2 -> Lives (applyBindings bindings t1) (applyBindings bindings t2)
  WorksAt t1 t2 -> WorksAt (applyBindings bindings t1) (applyBindings bindings t2)

-- Match a relation against the knowledge base
matchRelation :: KnowledgeBase -> Relation -> [Bindings]
matchRelation kb queryRel = 
  let facts = [rel | Fact rel <- kb]
  in concatMap (tryMatch queryRel) facts
  where
    tryMatch :: Relation -> Relation -> [Bindings]
    tryMatch query fact = case (query, fact) of
      (IsA qt1 qt2, IsA ft1 ft2) -> maybeToList $ combineBindings 
        <$> unify qt1 ft1 <*> unify qt2 ft2
      (Has qt1 qt2, Has ft1 ft2) -> maybeToList $ combineBindings 
        <$> unify qt1 ft1 <*> unify qt2 ft2
      (Loves qt1 qt2, Loves ft1 ft2) -> maybeToList $ combineBindings 
        <$> unify qt1 ft1 <*> unify qt2 ft2
      (Lives qt1 qt2, Lives ft1 ft2) -> maybeToList $ combineBindings 
        <$> unify qt1 ft1 <*> unify qt2 ft2
      (WorksAt qt1 qt2, WorksAt ft1 ft2) -> maybeToList $ combineBindings 
        <$> unify qt1 ft1 <*> unify qt2 ft2
      _ -> []
    
    combineBindings :: Bindings -> Bindings -> Bindings
    combineBindings b1 b2 = M.union b1 b2
    
    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- Evaluate a query
evaluate :: KnowledgeBase -> Query -> Bool
evaluate kb query = case query of
  QueryRelation rel -> not $ null $ matchRelation kb rel
  QueryExists term -> any (containsTerm term) [rel | Fact rel <- kb]
  where
    containsTerm :: Term -> Relation -> Bool
    containsTerm term rel = case rel of
      IsA t1 t2 -> term == t1 || term == t2
      Has t1 t2 -> term == t1 || term == t2
      Loves t1 t2 -> term == t1 || term == t2
      Lives t1 t2 -> term == t1 || term == t2
      WorksAt t1 t2 -> term == t1 || term == t2

-- Helper functions
parseText :: Parser a -> Text -> Either ParseError a
parseText parser input = parse parser "" input

-- Example usage and testing
sampleKB :: KnowledgeBase
sampleKB = 
  [ Fact (IsA (Entity "Hans") (Entity "Mann"))
  , Fact (IsA (Entity "Maria") (Entity "Frau"))  
  , Fact (Lives (Entity "Hans") (Entity "Berlin"))
  , Fact (WorksAt (Entity "Maria") (Entity "Google"))
  , Fact (Loves (Entity "Hans") (Entity "Maria"))
  , Fact (Has (Entity "Hans") (Entity "Hund"))
  ]

-- Test functions
testParse :: IO ()
testParse = do
  putStrLn "Testing German ACE Parser:"
  
  let testStatements = 
        [ "Hans ist Mann."
        , "Maria arbeitet bei Google."
        , "Hans liebt Maria."
        , "Wenn X ist Mann und X liebt Y dann Y ist glücklich."
        ]
  
  mapM_ (\stmt -> do
    putStrLn $ "Parsing: " ++ T.unpack stmt
    case parseText parseStatement stmt of
      Left err -> putStrLn $ "Error: " ++ show err
      Right result -> putStrLn $ "Success: " ++ show result
    putStrLn ""
    ) testStatements

testQueries :: IO ()
testQueries = do
  putStrLn "Testing Queries:"
  
  let queries = 
        [ "Ist es wahr dass Hans ist Mann?"
        , "Ist es wahr dass Maria arbeitet bei Google?"
        , "Existiert Hans?"
        , "Ist es wahr dass Hans liebt Berlin?"
        ]
  
  mapM_ (\q -> do
    putStrLn $ "Query: " ++ T.unpack q
    case parseText parseQuery q of
      Left err -> putStrLn $ "Parse Error: " ++ show err
      Right query -> do
        let result = evaluate sampleKB query
        putStrLn $ "Result: " ++ show result
    putStrLn ""
    ) queries

-- Main function to run tests
main :: IO ()
main = do
  testParse
  putStrLn "----------------------------------------"
  testQueries
