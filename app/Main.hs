{-# LANGUAGE FlexibleInstances #-}

module Main where
import Control.Exception

data Token =
   Rec | Enum | Fn | Ref String | Assign |
   Arrow | Dot | Module | Colon | Comma | With |
   Tick | Retn deriving (Eq, Show)


matchPrefix (a : b) (c : d)
   | a == c = matchPrefix b d
   | otherwise = False
matchPrefix [] _ = True
matchPrefix _ [] = False

takeStripPrefixWhile cond source = aux [] source cond where
   aux result [] _ = Nothing
   aux result l@(a : b) cond =
      if cond a
      then aux (result ++ [a]) b cond
      else Just (result, l)

data ASTGenError =
   SyntacticError String | ImplementationError String |
   ParseError String deriving (Eq, Show)

instance Exception ASTGenError

extractName :: String -> Maybe (String, String)
extractName ('\'' : b) = do
  (name, (_ : rest)) <- takeStripPrefixWhile (/= '\'') b
  return (name, rest)
extractName a = takeStripPrefixWhile (\e -> not (e `oneOf` ("{:" ++ delimiters))) a

data State =
   ModAnchor | ObjBodyDefn | WithAnchor | WithBodyDefn |
   ModuleBodyDefn | ClosureGuts | FnDeclSiteType |
   Var | LVal | RVal | ObjAnchor | Name | DerivationResolution deriving (Eq, Show)

makeErrorAboutTrailingWS :: String -> Int -> String
makeErrorAboutTrailingWS kw ln =
   "Syntactic error encountered at line " ++ show ln ++
   ". Keyword '" ++ kw ++ "' must have a trailing whitespace."


takeStripPrefix :: Int -> [a] -> Maybe ([a], [a])
takeStripPrefix n s = aux n [] s where
   aux 0 ss rs = Just (ss, rs)
   aux n ss (a : b) = aux (n - 1) (ss ++ [a]) b
   aux _ _ [] = Nothing


oneOf a (b : c) | a == b = True | otherwise = oneOf a c
oneOf _ [] = False

countBy :: Eq a => (a -> Bool) -> [a] -> Int
countBy pred s = aux pred s 0 where
   aux p (a : b) c
      | p a = aux p b (c + 1)
      | otherwise = aux p b c
   aux _ [] c = c

lineposErrHeader ln =
  "Error: At line " ++ show  ln

delimiters = "\t\n "

attempt :: Maybe a -> err -> Either err a
attempt (Just v) _ = Right v
attempt Nothing e = Left e

instance MonadFail (Either ASTGenError) where
  fail errmsg = Left (ImplementationError errmsg)

firstMatchPrefix m (a : b) | m a = True | otherwise = firstMatchPrefix m b
firstMatchPrefix _ [] = False

tokenise :: String -> Either ASTGenError [Token]
tokenise input = aux input [] [ModAnchor] 0 where
   aux :: String -> [Token] -> [State] -> Int -> Either ASTGenError [Token]
   aux [] list _ _ =
     if list == [] then Left (ParseError "Given file is empty") else Right (reverse list)
   aux ('-' : '-' : b) tl st ln =
      let r = takeStripPrefixWhile (/= '\n') b
      in case r of
         Just (_, cont) -> aux cont tl st (ln + 1)
         Nothing -> Left (SyntacticError (
            (lineposErrHeader ln) ++ " file unexpectedly ended"))
   aux (tr : b) tl st ln
      | tr == ' ' || tr == '\t' = aux b tl st ln
      | tr == '\n' = aux b tl st (ln + 1)
   aux s tl st@(Name : b : _) ln
      | b == ObjAnchor || b == ModAnchor =
         case extractName s of
            Just (name, rest) ->
               if elem '#' name
               then Left (SyntacticError (
                 (lineposErrHeader ln) ++
                 " name of an object is specified as a special function name." ++
                 " It is forbidden"))
               else if length name /= 0 then aux rest (Ref name : tl) ((case b of
                  ModAnchor -> ModuleBodyDefn
                  ObjAnchor -> ObjBodyDefn) : st) ln
               else Left (ParseError ((lineposErrHeader ln) ++ " object cannot be nameless"))
            Nothing -> Left (SyntacticError (
               (lineposErrHeader ln) ++
               " valid name is expected but not recognised"))
      | otherwise = Left (SyntacticError (
         (lineposErrHeader ln) ++ " name preceeds non-body component"))
   aux ('m' : 'o' : 'd' : 'u' : 'l' : 'e' : s : rest) tokenList state@(ModAnchor : _) lineNumber =
      if not (s `oneOf` delimiters)
      then Left (SyntacticError ((lineposErrHeader lineNumber) ++
        " keyword 'module' is not separated by a delimiter from name. Put one between"))
      else aux rest (Module : tokenList) (Name : state) lineNumber

   aux ('{' : rest) tokens state@(b : _) line
      | b == ObjBodyDefn || b == ModuleBodyDefn =
      case takeStripPrefixWhile (/= '}') rest of
        Just (pref, (_ : cont)) ->
          let newLines = countBy (== '\n') (tail pref)
              hasDeclsInBody = firstMatchPrefix (\e -> not (e `oneOf` delimiters)) pref
          in if hasDeclsInBody
            then Left (ParseError ((lineposErrHeader line) ++
              " something is inside object declaration. " ++
              "Currently cannot parse things there"))
            else aux cont tokens (drop 3 state) (line + newLines)
        Nothing -> Left (ParseError (
          (lineposErrHeader line) ++ " text unexpectedly abrupted"))
      | otherwise = aux rest tokens state line

   aux l _ state ln = Left (ImplementationError (
    "At line " ++ show ln ++ " with state history " ++ show state ++ " have " ++ show l ++
    " left and dont know what to do with it"))

testTrivial =
   ["module Ex{}", "\t\t\n\nmodule\n\t\n\t Ex\n\t\n\t {\n\t\n\t}"]

testComplete =
   "module Ex {\n" ++
   "\tfn 'count symbols in #' : String -> Int\n" ++
   "\twith L, R {\n\t rec Pair {left: L, rigth: R}\n\t" ++
   "enum Either {left: L, rigth: R}\n" ++ "}\n" ++ "}"

infixl |>
value |> function = function value

rigths v = aux v [] where
  aux (Right a : b) c = aux b (a : c)
  aux (Left _ : b) c = aux b c
  aux [] c = c

main = do
  let target = repeat [Module, Ref "Ex"]
  let testResults = map tokenise testTrivial |> rigths
  let result = zip target testResults |> map (\(l,r) -> l == r) |> foldr (&&) True
  let message = if not result then "Some tests have failed" else "All ok, tests have been passed ;)"
  print message