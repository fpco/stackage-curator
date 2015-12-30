{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Manipulate @GenericPackageDescription@ from Cabal into something more
-- useful for us.
module Stackage.PackageDescription
    ( SimpleDesc (..)
    , toSimpleDesc
    , CheckCond (..)
    , Component (..)
    , DepInfo (..)
    ) where

import           Control.Monad.Writer.Strict     (MonadWriter, execWriterT,
                                                  tell)
import           Distribution.Compiler           (CompilerFlavor)
import           Distribution.Package            (Dependency (..))
import           Distribution.PackageDescription
import           Distribution.System             (Arch, OS)
import           Stackage.PackageIndex
import           Stackage.Prelude

-- | Convert a 'GenericPackageDescription' into a 'SimpleDesc' by following the
-- constraints in the provided 'CheckCond'.
toSimpleDesc :: MonadThrow m
             => CheckCond
             -> SimplifiedPackageDescription
             -> m SimpleDesc
toSimpleDesc cc spd = execWriterT $ do
    forM_ (spdCondLibrary spd) $ tellTree cc CompLibrary
    forM_ (spdCondExecutables spd) $ tellTree cc CompExecutable . snd
    tell mempty { sdProvidedExes = setFromList
                                 $ map (fromString . fst)
                                 $ spdCondExecutables spd
                }
    when (ccIncludeTests cc) $ forM_ (spdCondTestSuites spd)
        $ tellTree cc CompTestSuite . snd
    when (ccIncludeBenchmarks cc) $ forM_ (spdCondBenchmarks spd)
        $ tellTree cc CompBenchmark . snd

-- | Convert a single CondTree to a 'SimpleDesc'.
tellTree :: (MonadWriter SimpleDesc m, MonadThrow m)
         => CheckCond
         -> Component
         -> CondTree ConfVar [Dependency] SimplifiedComponentInfo
         -> m ()
tellTree cc component =
    loop
  where
    loop (CondNode dat deps comps) = do
        tell mempty
            { sdPackages = unionsWith (<>) $ flip map deps
                $ \(Dependency x y) -> singletonMap x DepInfo
                    { diComponents = singletonSet component
                    , diRange = simplifyVersionRange y
                    }
            , sdTools = unionsWith (<>) $ flip map (sciBuildTools dat)
                $ \(Dependency name range) -> singletonMap
                    -- In practice, cabal files refer to the exe name, not the
                    -- package name.
                    (ExeName $ unPackageName name)
                    DepInfo
                        { diComponents = singletonSet component
                        , diRange = simplifyVersionRange range
                        }
            , sdModules = sciModules dat
            }
        forM_ comps $ \(cond, ontrue, onfalse) -> do
            b <- checkCond cc cond
            if b
                then loop ontrue
                else maybe (return ()) loop onfalse

-- | Resolve a condition to a boolean based on the provided 'CheckCond'.
checkCond :: MonadThrow m => CheckCond -> Condition ConfVar -> m Bool
checkCond CheckCond {..} cond0 =
    go cond0
  where
    go (Var (OS os)) = return $ os == ccOS
    go (Var (Arch arch)) = return $ arch == ccArch
    go (Var (Flag flag)) =
        case lookup flag ccFlags of
            Nothing -> throwM $ FlagNotDefined ccPackageName flag cond0
            Just b -> return b
    go (Var (Impl flavor range)) = return
                                 $ flavor == ccCompilerFlavor
                                && ccCompilerVersion `withinRange` range
    go (Lit b) = return b
    go (CNot c) = not `liftM` go c
    go (CAnd x y) = (&&) `liftM` go x `ap` go y
    go (COr x y) = (||) `liftM` go x `ap` go y

data CheckCondException = FlagNotDefined PackageName FlagName (Condition ConfVar)
    deriving (Show, Typeable)
instance Exception CheckCondException

data CheckCond = CheckCond
    { ccPackageName       :: PackageName -- for debugging only
    , ccOS                :: OS
    , ccArch              :: Arch
    , ccFlags             :: Map FlagName Bool
    , ccCompilerFlavor    :: CompilerFlavor
    , ccCompilerVersion   :: Version
    , ccIncludeTests      :: Bool
    , ccIncludeBenchmarks :: Bool
    }
