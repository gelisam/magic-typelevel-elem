#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , directory
             , filepath
             , text
             , time
             , typed-process
-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

import Data.Function ((&))
import Data.Time.Clock
import Data.Foldable (for_)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((<.>), (</>))
import System.Process.Typed (ProcessConfig)
import qualified System.Directory as FilePath
import qualified System.Process.Typed as Process


main
  :: IO ()
main = do
  getArgs >>= \case
    [arg] | not (arg `elem` ["-h", "--help"]) -> do
      -- "/..." (root of the repo)
      let rootPath :: FilePath
          rootPath = arg

      -- "/.../benchmark-cases"
      let casesDir :: FilePath
          casesDir = rootPath </> "benchmark-cases"

      -- ["no-magic", ...]
      caseSubdirs <- FilePath.listDirectory casesDir

      for_ caseSubdirs $ \caseSubdir -> do
        -- "no-magic"
        let caseName :: String
            caseName = caseSubdir

        -- "/.../benchmark-cases/no-magic"
        let workingDir :: FilePath
            workingDir = casesDir </> caseSubdir

        -- "/.../benchmark-cases/no-magic/app/Main.hs"
        let mainPath :: FilePath
            mainPath = workingDir </> "app/Main.hs"

        -- "/.../benchmark-cases/no-magic/app/Main.hs.prefix"
        let mainPrefixPath :: FilePath
            mainPrefixPath = mainPath <.> "prefix"

        -- stack build
        let processConfig :: ProcessConfig () () ()
            processConfig = Process.proc "stack" ["build"]
                          & Process.setStdin Process.nullStream
                          & Process.setStdout Process.nullStream
                          & Process.setStderr Process.nullStream
                          & Process.setWorkingDir workingDir

        let timeBuild :: Int -> IO Double
            timeBuild n = do
              startTime <- getCurrentTime
              mainPrefix <- lines <$> readFile mainPrefixPath
              let mainSuffix =
                    [ "main = pure (unit @" ++ show n ++ " @'" ++ show [1..n] ++ ")"
                    ]
              let mainCode = mainPrefix ++ mainSuffix
              writeFile mainPath (unlines mainCode)
              Process.runProcess_ processConfig
              finishTime <- getCurrentTime
              pure $ realToFrac  (diffUTCTime finishTime startTime)

        -- warm the cache
        _ <- timeBuild 2

        -- run the benchmark
        putStrLn caseName
        for_ [100,200..2000] $ \n -> do
          t <- timeBuild n
          print (n, t)
    _ -> do
      putStrLn "usage: "
      putStrLn $ "  ./Benchmark.hs `pwd`"
      putStrLn ""
      putStrLn "Build a few projects with larger and larger type-level lists,"
      putStrLn "to compare the performance of various approaches."
