{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Parser (parseFuthark)
import System.Exit
import System.IO
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Futhark.Compiler
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark
import Data.Foldable (toList)

data DefKind = Value | Module | ModuleType | Type

data Def = Def DefKind Name Loc

kindText :: DefKind -> T.Text
kindText Value = "value"
kindText Module = "module"
kindText ModuleType = "module type"
kindText Type = "type"

printDef :: Def -> IO ()
printDef (Def k name loc) = do
  T.putStrLn $ T.unwords [kindText k, nameToText name, T.pack (locStr loc)]

defsInProg :: UncheckedProg -> Seq.Seq Def
defsInProg = foldMap defsInDec . progDecs
  where
    defsInDec (ValDec vb) =
      Seq.singleton $ Def Value (valBindName vb) (locOf vb)
    defsInDec (TypeDec tb) =
      Seq.singleton $ Def Type (typeAlias tb) (locOf tb)
    defsInDec (LocalDec d _) = defsInDec d
    defsInDec (OpenDec me _) = defsInModExp me
    defsInDec (ModDec mb) = defsInModExp $ modExp mb
    defsInDec SigDec {} = mempty
    defsInDec ImportDec {} = mempty

    defsInModExp ModVar {} = mempty
    defsInModExp (ModParens me _) = defsInModExp me
    defsInModExp ModImport {} = mempty
    defsInModExp (ModDecs ds _) = foldMap defsInDec ds
    defsInModExp (ModApply me1 me2 _ _ _) = defsInModExp me1 <> defsInModExp me2
    defsInModExp (ModAscript me _ _ _) = defsInModExp me
    defsInModExp (ModLambda _ _ me _) = defsInModExp me

printElements :: Seq.Seq Def -> IO()
printElements = mapM_ printDef . toList

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- T.readFile file
      case parseFuthark file s of
        Left e -> do
          exitWith $ ExitFailure 2
        Right prog -> do
          --T.putStrLn $ prettyText prog
          --printElements $ defsInProg prog
          case progDoc prog of
            Nothing -> do
              T.putStrLn $ "No documentation found."
              exitWith $ ExitFailure 2
            Just (DocComment com _) -> do
              T.putStrLn $ prettyText com
          -- use list produced in printElements (instead of just printing)
          -- index into loc of each def and check if there has been a comment before a.k.a. flush
            
          -- TODO: how do we properly access the Token list of Comments from 
          -- ParserState in monad.hs?
    _ -> Nothing