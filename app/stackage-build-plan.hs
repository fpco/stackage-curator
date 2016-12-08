{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Data.Aeson                (toJSON)
import           Data.Aeson.Text           (encodeToTextBuilder)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Text.Lazy.IO         as TLIO
import           Options.Applicative
import           Options.Applicative.Simple (simpleOptions, simpleVersion)
import           Paths_stackage_curator    (version)
import           Stackage.ShowBuildPlan
import           Data.Monoid               ((<>))

main :: IO ()
main = do
    ((set, render, packages), ()) <- simpleOptions
        $(simpleVersion version)
        "Calculate and print (in different formats) Stackage build plans"
        "Calculate and print (in different formats) Stackage build plans"
        options
        empty
    tis <- getBuildPlan set $ map (mkPackageName . T.pack) packages
    TLIO.putStr $ render set tis
  where
    options = (,,)
        <$> setOptions
        <*> renderOptions
        <*> some (argument str (metavar "PACKAGES..."))

    mkSettings mmirror msnapshot mcommands =
          maybe id (setMirror . T.pack) mmirror
        $ maybe id setSnapshot msnapshot
        $ maybe id setShellCommands mcommands
        $ defaultSettings

    setOptions = mkSettings
        <$> ((fmap Just $ strOption
            ( long "mirror"
           <> help "Mirror to download packages from"
           <> metavar "URL"
            )) <|> pure Nothing)
        <*> ((fmap Just $ option readSnapshot
            ( long "snapshot"
           <> help "Which snapshot to pull the build plan from"
           <> metavar "SNAPSHOT"
            )) <|> pure Nothing)
        <*> ((fmap Just $ option readCommands
            ( long "commands"
           <> help "Shell commands do use: abstract, simple"
           <> metavar "COMMANDS"
            )) <|> pure Nothing)

    readSnapshot =
        str >>=
        either (fail . show) return . parseSnapshotSpec . T.pack

    readCommands = do
        s <- str
        case s of
            "abstract" -> return abstractCommands
            "simple" -> return simpleCommands
            _ -> fail $ "Invalid commands: " ++ s

    renderOptions =
        (option readRender
            ( long "format"
           <> help "Output format: shell, simple, json"
           <> metavar "FORMAT"
           ))

    readRender = do
        x <- str
        case x of
            "shell" -> return $ \set tis ->
                TL.fromStrict $ toShellScript set tis
            "simple" -> return $ \_set tis ->
                TL.fromStrict $ toSimpleText tis
            "json" -> return $ \_set tis ->
                toLazyText $ encodeToTextBuilder $ toJSON tis
            _ -> fail $ "Invalid renderer: " ++ x
