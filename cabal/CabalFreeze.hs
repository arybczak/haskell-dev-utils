{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Exit
import System.Process
import qualified Data.Attoparsec.Combinator as C
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Version = Version { unVersion :: [Int] }
  deriving (Eq, Ord)

instance Show Version where
  show = intercalate "." . map show . unVersion

showVersion :: Version -> T.Text
showVersion = T.pack . show

showMaybeVersion :: Maybe Version -> T.Text
showMaybeVersion Nothing  = "none"
showMaybeVersion (Just v) = showVersion v

----------------------------------------

type CabalConfig = [(T.Text, Version)]

type PackageSetDelta = M.Map T.Text (Maybe Version, Maybe Version)

parseCabalConfig :: T.Text -> CabalConfig
parseCabalConfig = either error id
  . P.parseOnly (P.asciiCI "constraints:" *> C.sepBy1 parser (P.char ','))
  where
    parser = (,)
      <$ P.skipSpace
      <*> P.takeWhile (not . isSpace)
      <* P.skipSpace
      <* P.skipWhile (== '=')
      <* P.skipSpace
      <*> parseVersion
      <* P.skipSpace

    parseVersion = Version <$> C.sepBy1 P.decimal (P.char '.')

----------------------------------------

computeDelta :: CabalConfig -> CabalConfig -> PackageSetDelta
computeDelta cc cc' = M.unionWith combine
  (toDelta (\v -> (Just v, Nothing)) cc )
  (toDelta (\v -> (Nothing, Just v)) cc')
  where
    combine (v1, Nothing) (Nothing, v2) = (v1, v2)
    combine _ _ = error "computeDelta: bad values"

    toDelta f = M.fromList . map (second f)

showDelta :: PackageSetDelta -> T.Text
showDelta = T.unlines . map stringify . filter differentVersion . M.toAscList
  where
    differentVersion (_, (v1, v2)) = v1 /= v2

    stringify (name, (v1, v2)) = T.concat [
        name
      , ": "
      , showMaybeVersion v1
      , " -> "
      , showMaybeVersion v2
      ]

deltaToCabalConfig :: PackageSetDelta -> T.Text
deltaToCabalConfig = T.concat
  . (++ ["\n"])
  . ("constraints: " :)
  . intersperse ",\n             "
  . map stringify
  . filter (not . isRemoved)
  . M.toAscList
  where
    isRemoved (_, (_, v)) = isNothing v

    stringify (name, version) = T.concat [
        name
      , " =="
      , case version of
          -- Supress version bumps of existing packages.
          (Just v, _) -> showVersion v
          (_, Just v) -> showVersion v
          _           -> error "deltaTocabalConfig: no version, impossible"
      ]

----------------------------------------

addedPackages :: PackageSetDelta -> PackageSetDelta
addedPackages = M.filter $ \(v1, _) -> v1 == Nothing

removedPackages :: PackageSetDelta -> PackageSetDelta
removedPackages = M.filter $ \(_, v2) -> v2 == Nothing

changedPackages :: PackageSetDelta -> PackageSetDelta
changedPackages = M.filter $ \(v1, v2) -> isJust v1 && isJust v2 && v1 /= v2

excludedPackages :: PackageSetDelta -> PackageSetDelta
excludedPackages = M.filterWithKey $ \name _ -> name `elem` excludedList
  where
    excludedList :: [T.Text]
    excludedList = [
        "Cabal"
      , "StateVar"
      , "array"
      , "base"
      , "bin-package-db"
      , "containers"
      , "deepseq"
      , "directory"
      , "ghc"
      , "ghc-boot"
      , "ghc-boot-th"
      , "ghc-prim"
      , "integer-gmp"
      , "integer-simple"
      , "pretty"
      , "rts"
      , "template-haskell"
      , "unix"
      ]

----------------------------------------

main :: IO ()
main = do
  config <- parseCabalConfig <$> T.readFile configFile
  copyFile configFile oldConfigFile
  system "cabal freeze" >>= \case
    ExitFailure code -> putStrLn $ "cabal freeze failed with exit code " ++ show code
    ExitSuccess -> do
      newConfig <- parseCabalConfig <$> T.readFile configFile
      let delta     = computeDelta config newConfig
          excluded  = excludedPackages delta
          realDelta = delta `M.difference` excluded
      T.putStrLn "EXCLUDED:"
      T.putStrLn $ showDelta excluded
      T.putStrLn "ADDED:"
      T.putStrLn $ showDelta $ addedPackages realDelta
      T.putStrLn "REMOVED:"
      T.putStrLn $ showDelta $ removedPackages realDelta
      T.putStrLn "SUPRESSED CHANGES:"
      T.putStrLn $ showDelta $ changedPackages realDelta
      T.writeFile configFile $ deltaToCabalConfig realDelta
  where
    configFile    = "cabal.config"
    oldConfigFile = "cabal.config.old"
