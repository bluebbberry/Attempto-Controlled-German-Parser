{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (find, nub)
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Combinator as P
import Control.Monad (guard)

-- Enhanced Abstract Syntax Tree
data Term =
    Entity Text
  | Variable Text
  | Number Int
  deriving (Show, Eq, Ord)

data Relation =
    IsA Term Term           -- "X ist ein Y"
  | Has Term Term           -- "X hat Y"
  | Loves Term Term         -- "X liebt Y"
  | Lives Term Term         -- "X lebt in Y"
  | WorksAt Term Term       -- "X arbeitet bei Y"
  | Knows Term Term         -- "X kennt Y"
  | Teaches Term Term       -- "X unterrichtet Y"
  | Owns Term Term          -- "X besitzt Y"
  | GreaterThan Term Term   -- "X ist größer als Y"
  | SmallerThan Term Term   -- "X ist kleiner als Y"
  | Equal Term Term         -- "X ist gleich Y"
  | PartOf Term Term        -- "X ist Teil von Y"
  | Near Term Term          -- "X ist nah bei Y"
  deriving (Show, Eq)

data Statement =
    Fact Relation
  | Rule [Relation] Relation  -- if conditions then conclusion
  | Constraint [Relation]     -- constraints that must hold
  deriving (Show, Eq)

-- Enhanced Query Types
data Query =
    QueryRelation Relation
  | QueryExists Term
  | QueryAll Term Relation    -- "Alle X die..."
  | QueryWho Relation         -- "Wer..."
  | QueryWhat Relation        -- "Was..."
  | QueryWhere Term           -- "Wo ist X?"
  | QueryHow Relation         -- "Wie..."
  | QueryCount Relation       -- "Wie viele..."
  | QueryNot Relation         -- "Ist es nicht wahr dass..."
  deriving (Show, Eq)

-- Knowledge Base with inference capabilities
data KnowledgeBase = KB
  { facts :: [Relation]
  , rules :: [(Relation, [Relation])]  -- (conclusion, conditions)
  , constraints :: [[Relation]]
  } deriving (Show)

-- Enhanced Lexer
spaces1 :: Parser ()
spaces1 = skipMany1 space

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- Extended German vocabulary
article :: Parser Text
article = choice $ map (\s -> T.pack <$> try (string s))
  ["der", "die", "das", "ein", "eine", "einem", "einer", "den", "dem",
   "alle", "jeder", "jede", "jedes", "welcher", "welche", "welches"]

noun :: Parser Text
noun = choice $ map (\s -> T.pack <$> try (string s))
  ["Mann", "Frau", "Hund", "Katze", "Haus", "Stadt", "Unternehmen",
   "Person", "Tier", "Gebäude", "Programmierer", "Arzt", "Student",
   "Lehrer", "Auto", "Buch", "Computer", "Universität", "Firma",
   "Kind", "Eltern", "Freund", "Nachbar", "Chef", "Kollege", "Menschen"]

properNoun :: Parser Text
properNoun = choice $ map (\s -> T.pack <$> try (string s))
  ["Hans", "Maria", "Berlin", "München", "Google", "BMW", "Rex", "Fluffy",
   "Anna", "Peter", "Hamburg", "Köln", "Microsoft", "Apple", "Max", "Lisa",
   "Deutschland", "glücklich"]

verb :: Parser Text
verb = choice $ map (\s -> T.pack <$> try (string s))
  ["ist", "hat", "liebt", "lebt", "arbeitet", "kennt", "unterrichtet",
   "besitzt", "wohnt", "studiert", "lehrt", "kauft", "verkauft"]

adjective :: Parser Text
adjective = choice $ map (\s -> T.pack <$> try (string s))
  ["größer", "kleiner", "gleich", "alt", "jung", "groß", "klein",
   "intelligent", "freundlich", "reich", "arm"]

preposition :: Parser Text
preposition = choice $ map (\s -> T.pack <$> try (string s))
  ["in", "bei", "von", "zu", "mit", "an", "auf", "unter", "über",
   "neben", "zwischen", "vor", "hinter", "nah"]

questionWord :: Parser Text
questionWord = choice $ map (\s -> T.pack <$> try (string s))
  ["wer", "was", "wo", "wie", "wann", "warum", "welcher", "welche", "welches"]

-- Enhanced term parsing
parseTerm :: Parser Term
parseTerm = choice
  [ Entity <$> properNoun
  , Entity <$> noun
  , Variable <$> (T.pack <$> (string "X" <|> string "Y" <|> string "Z" <|> string "W"))
  , Number <$> (read <$> many1 digit)
  ]

-- Enhanced relation parsing
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

parseKnows :: Parser Relation
parseKnows = do
  subj <- parseTerm
  spaces1
  _ <- string "kennt"
  spaces1
  obj <- parseTerm
  return $ Knows subj obj

parseTeaches :: Parser Relation
parseTeaches = do
  subj <- parseTerm
  spaces1
  _ <- string "unterrichtet"
  spaces1
  obj <- parseTerm
  return $ Teaches subj obj

parseOwns :: Parser Relation
parseOwns = do
  subj <- parseTerm
  spaces1
  _ <- string "besitzt"
  spaces1
  obj <- parseTerm
  return $ Owns subj obj

parseGreaterThan :: Parser Relation
parseGreaterThan = do
  subj <- parseTerm
  spaces1
  _ <- string "ist"
  spaces1
  _ <- string "größer"
  spaces1
  _ <- string "als"
  spaces1
  obj <- parseTerm
  return $ GreaterThan subj obj

parseSmallerThan :: Parser Relation
parseSmallerThan = do
  subj <- parseTerm
  spaces1
  _ <- string "ist"
  spaces1
  _ <- string "kleiner"
  spaces1
  _ <- string "als"
  spaces1
  obj <- parseTerm
  return $ SmallerThan subj obj

parseEqual :: Parser Relation
parseEqual = do
  subj <- parseTerm
  spaces1
  _ <- string "ist"
  spaces1
  _ <- string "gleich"
  spaces1
  obj <- parseTerm
  return $ Equal subj obj

parsePartOf :: Parser Relation
parsePartOf = do
  subj <- parseTerm
  spaces1
  _ <- string "ist"
  spaces1
  _ <- string "Teil"
  spaces1
  _ <- string "von"
  spaces1
  obj <- parseTerm
  return $ PartOf subj obj

parseNear :: Parser Relation
parseNear = do
  subj <- parseTerm
  spaces1
  _ <- string "ist"
  spaces1
  _ <- string "nah"
  spaces1
  _ <- string "bei"
  spaces1
  obj <- parseTerm
  return $ Near subj obj

parseRelation :: Parser Relation
parseRelation = choice
  [ try parseGreaterThan
  , try parseSmallerThan
  , try parseEqual
  , try parsePartOf
  , try parseNear
  , try parseIsA
  , try parseHas
  , try parseLoves
  , try parseLives
  , try parseWorksAt
  , try parseKnows
  , try parseTeaches
  , try parseOwns
  ]

-- Enhanced statement parsing
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

parseConstraint :: Parser Statement
parseConstraint = do
  _ <- string "Es"
  spaces1
  _ <- string "muss"
  spaces1
  _ <- string "gelten"
  spaces1
  _ <- string "dass"
  spaces1
  constraints <- parseRelation `sepBy1` (spaces1 >> string "und" >> spaces1)
  _ <- string "."
  return $ Constraint constraints

parseStatement :: Parser Statement
parseStatement = choice [try parseRule, try parseConstraint, parseFact]

-- Enhanced query parsing
parseQueryRelation :: Parser Query
parseQueryRelation = do
  _ <- string "Ist"
  spaces1
  _ <- string "es"
  spaces1
  _ <- string "wahr"
  spaces1
  _ <- string "dass"
  spaces1
  rel <- parseRelation
  _ <- string "?"
  return $ QueryRelation rel

parseQueryNot :: Parser Query
parseQueryNot = do
  _ <- string "Ist"
  spaces1
  _ <- string "es"
  spaces1
  _ <- string "nicht"
  spaces1
  _ <- string "wahr"
  spaces1
  _ <- string "dass"
  spaces1
  rel <- parseRelation
  _ <- string "?"
  return $ QueryNot rel

parseQueryExists :: Parser Query
parseQueryExists = do
  _ <- string "Existiert"
  spaces1
  term <- parseTerm
  _ <- string "?"
  return $ QueryExists term

parseQueryWho :: Parser Query
parseQueryWho = do
  _ <- string "Wer"
  spaces1
  -- Parse the rest as a relation but expect a variable in subject position
  rel <- parseRelationWithVariable
  _ <- string "?"
  return $ QueryWho rel
  where
    parseRelationWithVariable = do
      _ <- string "arbeitet"
      spaces1
      _ <- string "bei"
      spaces1
      obj <- parseTerm
      return $ WorksAt (Variable "X") obj

parseQueryWhat :: Parser Query
parseQueryWhat = do
  _ <- string "Was"
  spaces1
  -- Parse the rest as a relation but expect a variable in object position
  rel <- parseRelationWithVariableObject
  _ <- string "?"
  return $ QueryWhat rel
  where
    parseRelationWithVariableObject = do
      _ <- string "hat"
      spaces1
      subj <- parseTerm
      return $ Has subj (Variable "Y")

parseQueryWhere :: Parser Query
parseQueryWhere = do
  _ <- string "Wo"
  spaces1
  _ <- string "ist"
  spaces1
  term <- parseTerm
  _ <- string "?"
  return $ QueryWhere term

parseQueryAll :: Parser Query
parseQueryAll = do
  _ <- string "Alle"
  spaces1
  termType <- parseTerm  -- The type we're looking for (e.g., "Mann")
  spaces1
  _ <- string "die"
  spaces1
  rel <- parseRelationForAll termType
  _ <- string "?"
  return $ QueryAll termType rel
  where
    parseRelationForAll termType = do
      verb <- choice [string "liebt", string "kennt", string "hat"]
      spaces1
      obj <- parseTerm
      case verb of
        "liebt" -> return $ Loves (Variable "X") obj
        "kennt" -> return $ Knows (Variable "X") obj
        "hat" -> return $ Has (Variable "X") obj
        _ -> fail "Unknown verb"

parseQueryCount :: Parser Query
parseQueryCount = do
  _ <- string "Wie"
  spaces1
  _ <- string "viele"
  spaces1
  -- Parse a noun/term that we're counting
  term <- parseTerm
  spaces1
  rel <- parseRelationForCount term
  _ <- string "?"
  return $ QueryCount rel
  where
    parseRelationForCount term = do
      _ <- string "arbeitet" <|> string "arbeiten"
      spaces1
      _ <- string "bei"
      spaces1
      obj <- parseTerm
      return $ WorksAt term obj

parseQuery :: Parser Query
parseQuery = choice
  [ try parseQueryNot
  , try parseQueryRelation
  , try parseQueryExists
  , try parseQueryWho
  , try parseQueryWhat
  , try parseQueryWhere
  , try parseQueryAll
  , try parseQueryCount
  ]

-- Enhanced unification and inference engine
type Bindings = Map Text Term

-- Improved unification
unify :: Term -> Term -> Maybe Bindings
unify (Variable x) t = Just $ M.singleton x t
unify t (Variable x) = Just $ M.singleton x t
unify (Entity a) (Entity b)
  | a == b = Just M.empty
  | otherwise = Nothing
unify (Number a) (Number b)
  | a == b = Just M.empty
  | otherwise = Nothing
unify _ _ = Nothing

-- Combine bindings with conflict checking
combineBindings :: Bindings -> Bindings -> Maybe Bindings
combineBindings b1 b2 =
  let commonKeys = S.intersection (M.keysSet b1) (M.keysSet b2)
      conflicts = S.filter (\k -> M.lookup k b1 /= M.lookup k b2) commonKeys
  in if S.null conflicts
     then Just $ M.union b1 b2
     else Nothing

-- Apply bindings to terms and relations
applyBindings :: Bindings -> Term -> Term
applyBindings bindings (Variable x) =
  case M.lookup x bindings of
    Just term -> term
    Nothing -> Variable x
applyBindings _ term = term

applyBindingsRel :: Bindings -> Relation -> Relation
applyBindingsRel bindings rel = case rel of
  IsA t1 t2 -> IsA (applyBindings bindings t1) (applyBindings bindings t2)
  Has t1 t2 -> Has (applyBindings bindings t1) (applyBindings bindings t2)
  Loves t1 t2 -> Loves (applyBindings bindings t1) (applyBindings bindings t2)
  Lives t1 t2 -> Lives (applyBindings bindings t1) (applyBindings bindings t2)
  WorksAt t1 t2 -> WorksAt (applyBindings bindings t1) (applyBindings bindings t2)
  Knows t1 t2 -> Knows (applyBindings bindings t1) (applyBindings bindings t2)
  Teaches t1 t2 -> Teaches (applyBindings bindings t1) (applyBindings bindings t2)
  Owns t1 t2 -> Owns (applyBindings bindings t1) (applyBindings bindings t2)
  GreaterThan t1 t2 -> GreaterThan (applyBindings bindings t1) (applyBindings bindings t2)
  SmallerThan t1 t2 -> SmallerThan (applyBindings bindings t1) (applyBindings bindings t2)
  Equal t1 t2 -> Equal (applyBindings bindings t1) (applyBindings bindings t2)
  PartOf t1 t2 -> PartOf (applyBindings bindings t1) (applyBindings bindings t2)
  Near t1 t2 -> Near (applyBindings bindings t1) (applyBindings bindings t2)

-- Enhanced pattern matching with unification
matchRelations :: Relation -> Relation -> Maybe Bindings
matchRelations query fact = case (query, fact) of
  (IsA qt1 qt2, IsA ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Has qt1 qt2, Has ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Loves qt1 qt2, Loves ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Lives qt1 qt2, Lives ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (WorksAt qt1 qt2, WorksAt ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Knows qt1 qt2, Knows ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Teaches qt1 qt2, Teaches ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Owns qt1 qt2, Owns ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (GreaterThan qt1 qt2, GreaterThan ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (SmallerThan qt1 qt2, SmallerThan ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Equal qt1 qt2, Equal ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (PartOf qt1 qt2, PartOf ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  (Near qt1 qt2, Near ft1 ft2) -> do
    b1 <- unify qt1 ft1
    b2 <- unify qt2 ft2
    combineBindings b1 b2
  _ -> Nothing

-- Forward chaining inference engine
forwardChain :: KnowledgeBase -> KnowledgeBase
forwardChain kb@(KB facts rules constraints) =
  let newFacts = concatMap (applyRule facts) rules
      allFacts = nub (facts ++ newFacts)
  in if length allFacts == length facts
     then kb  -- Fixed point reached
     else forwardChain (KB allFacts rules constraints)
  where
    applyRule :: [Relation] -> (Relation, [Relation]) -> [Relation]
    applyRule currentFacts (conclusion, conditions) =
      let bindings = findBindings currentFacts conditions M.empty
      in map (applyBindingsRel) bindings <*> [conclusion]

    findBindings :: [Relation] -> [Relation] -> Bindings -> [Bindings]
    findBindings _ [] bindings = [bindings]
    findBindings currentFacts (condition:rest) bindings =
      let matches = [b | fact <- currentFacts,
                         Just b <- [matchRelations condition fact],
                         Just combined <- [combineBindings bindings b]]
      in concatMap (findBindings currentFacts rest) matches

-- Enhanced query evaluation
evaluateQuery :: KnowledgeBase -> Query -> Either String [Text]
evaluateQuery kb query =
  let inferredKB = forwardChain kb
  in case query of
    QueryRelation rel ->
      if any (isJust . matchRelations rel) (facts inferredKB)
      then Right ["Ja"]
      else Right ["Nein"]

    QueryNot rel ->
      if any (isJust . matchRelations rel) (facts inferredKB)
      then Right ["Nein"]
      else Right ["Ja"]

    QueryExists term ->
      if any (containsTerm term) (facts inferredKB)
      then Right ["Ja"]
      else Right ["Nein"]

    QueryWho rel -> Right $ findSubjects rel (facts inferredKB)
    QueryWhat rel -> Right $ findObjects rel (facts inferredKB)

    QueryWhere term ->
      let locations = [showTerm loc | Lives t loc <- facts inferredKB, t == term]
      in Right locations

    QueryAll searchType pattern ->
      let matches = findAllOfType searchType pattern (facts inferredKB)
      in Right matches

    QueryCount rel ->
      let count = length $ filter (isJust . matchRelations rel) (facts inferredKB)
      in Right [T.pack $ show count]
  where
    isJust (Just _) = True
    isJust Nothing = False

    containsTerm :: Term -> Relation -> Bool
    containsTerm term rel = case rel of
      IsA t1 t2 -> term == t1 || term == t2
      Has t1 t2 -> term == t1 || term == t2
      Loves t1 t2 -> term == t1 || term == t2
      Lives t1 t2 -> term == t1 || term == t2
      WorksAt t1 t2 -> term == t1 || term == t2
      Knows t1 t2 -> term == t1 || term == t2
      Teaches t1 t2 -> term == t1 || term == t2
      Owns t1 t2 -> term == t1 || term == t2
      GreaterThan t1 t2 -> term == t1 || term == t2
      SmallerThan t1 t2 -> term == t1 || term == t2
      Equal t1 t2 -> term == t1 || term == t2
      PartOf t1 t2 -> term == t1 || term == t2
      Near t1 t2 -> term == t1 || term == t2

    findSubjects :: Relation -> [Relation] -> [Text]
    findSubjects query factList = nub [showTerm subj | fact <- factList,
                                        Just bindings <- [matchRelations query fact],
                                        subj <- extractSubjects fact]

    findObjects :: Relation -> [Relation] -> [Text]
    findObjects query factList = nub [showTerm obj | fact <- factList,
                                       Just bindings <- [matchRelations query fact],
                                       obj <- extractObjects fact]

    findAllOfType :: Term -> Relation -> [Relation] -> [Text]
    findAllOfType searchType pattern factList =
      let -- Find all entities of the search type
          entitiesOfType = [e | IsA e t <- factList, t == searchType]
          -- Check which ones satisfy the pattern
          satisfying = filter (\entity ->
            let instantiated = instantiatePattern pattern entity
            in any (isJust . matchRelations instantiated) factList
            ) entitiesOfType
      in map showTerm satisfying

    instantiatePattern :: Relation -> Term -> Relation
    instantiatePattern rel entity = case rel of
      Loves (Variable _) obj -> Loves entity obj
      Knows (Variable _) obj -> Knows entity obj
      Has (Variable _) obj -> Has entity obj
      WorksAt (Variable _) obj -> WorksAt entity obj
      _ -> rel

    findAllMatches :: Term -> Relation -> [Relation] -> [Text]
    findAllMatches term pattern factList =
      nub [showTerm t | fact <- factList,
                        Just bindings <- [matchRelations pattern fact],
                        t <- extractMatchingTerms term fact]

    extractSubjects :: Relation -> [Term]
    extractSubjects rel = case rel of
      IsA t1 _ -> [t1]
      Has t1 _ -> [t1]
      Loves t1 _ -> [t1]
      Lives t1 _ -> [t1]
      WorksAt t1 _ -> [t1]
      Knows t1 _ -> [t1]
      Teaches t1 _ -> [t1]
      Owns t1 _ -> [t1]
      GreaterThan t1 _ -> [t1]
      SmallerThan t1 _ -> [t1]
      Equal t1 _ -> [t1]
      PartOf t1 _ -> [t1]
      Near t1 _ -> [t1]

    extractObjects :: Relation -> [Term]
    extractObjects rel = case rel of
      IsA _ t2 -> [t2]
      Has _ t2 -> [t2]
      Loves _ t2 -> [t2]
      Lives _ t2 -> [t2]
      WorksAt _ t2 -> [t2]
      Knows _ t2 -> [t2]
      Teaches _ t2 -> [t2]
      Owns _ t2 -> [t2]
      GreaterThan _ t2 -> [t2]
      SmallerThan _ t2 -> [t2]
      Equal _ t2 -> [t2]
      PartOf _ t2 -> [t2]
      Near _ t2 -> [t2]

    extractMatchingTerms :: Term -> Relation -> [Term]
    extractMatchingTerms target rel =
      filter (/= target) (extractSubjects rel ++ extractObjects rel)

    showTerm :: Term -> Text
    showTerm (Entity t) = t
    showTerm (Variable t) = t
    showTerm (Number n) = T.pack $ show n

-- Helper functions
parseText :: Parser a -> Text -> Either ParseError a
parseText parser input = parse parser "" input

-- Enhanced sample knowledge base
sampleKB :: KnowledgeBase
sampleKB = KB
  { facts =
      [ IsA (Entity "Hans") (Entity "Mann")
      , IsA (Entity "Maria") (Entity "Frau")
      , IsA (Entity "Peter") (Entity "Mann")
      , IsA (Entity "Anna") (Entity "Frau")
      , Lives (Entity "Hans") (Entity "Berlin")
      , Lives (Entity "Maria") (Entity "München")
      , WorksAt (Entity "Maria") (Entity "Google")
      , WorksAt (Entity "Peter") (Entity "BMW")
      , Loves (Entity "Hans") (Entity "Maria")
      , Has (Entity "Hans") (Entity "Hund")
      , Knows (Entity "Hans") (Entity "Peter")
      , Teaches (Entity "Anna") (Entity "Student")
      , Owns (Entity "Peter") (Entity "Auto")
      , GreaterThan (Number 10) (Number 5)
      , PartOf (Entity "Berlin") (Entity "Deutschland")
      , Near (Entity "Hans") (Entity "Peter")
      ]
  , rules =
      [ (Loves (Variable "Y") (Variable "X"), [Loves (Variable "X") (Variable "Y")])  -- Love is symmetric
      , (Knows (Variable "Y") (Variable "X"), [Knows (Variable "X") (Variable "Y")]) -- Knowing is symmetric
      , (IsA (Variable "X") (Entity "Person"), [IsA (Variable "X") (Entity "Mann")])
      , (IsA (Variable "X") (Entity "Person"), [IsA (Variable "X") (Entity "Frau")])
      ]
  , constraints = []
  }

-- Enhanced test functions
testParse :: IO ()
testParse = do
  putStrLn "Testing Enhanced German ACE Parser:"

  let testStatements =
        [ "Hans ist Mann."
        , "Maria arbeitet bei Google."
        , "Hans liebt Maria."
        , "Peter kennt Anna."
        , "Anna unterrichtet Student."
        , "Hans ist größer als Maria."
        , "Berlin ist Teil von Deutschland."
        , "Wenn X ist Mann und X liebt Y dann Y ist glücklich."
        , "Es muss gelten dass Hans kennt Maria."
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
  putStrLn "Testing Enhanced Queries:"

  let queries =
        [ "Ist es wahr dass Hans ist Mann?"
        , "Ist es nicht wahr dass Hans ist Frau?"
        , "Wer arbeitet bei Google?"
        , "Was hat Hans?"
        , "Wo ist Hans?"
        , "Existiert Peter?"
        , "Wie viele Menschen arbeitet bei Google?"
        , "Alle Mann die liebt Maria?"
        ]

  mapM_ (\q -> do
    putStrLn $ "Query: " ++ T.unpack q
    case parseText parseQuery q of
      Left err -> putStrLn $ "Parse Error: " ++ show err
      Right query -> do
        let result = evaluateQuery sampleKB query
        case result of
          Left err -> putStrLn $ "Evaluation Error: " ++ err
          Right answers -> putStrLn $ "Result: " ++ show answers
    putStrLn ""
    ) queries

-- Main function
main :: IO ()
main = do
  testParse
  putStrLn "----------------------------------------"
  testQueries
  putStrLn "----------------------------------------"
  putStrLn "Knowledge Base after forward chaining:"
  let inferredKB = forwardChain sampleKB
  mapM_ (putStrLn . show) (facts inferredKB)