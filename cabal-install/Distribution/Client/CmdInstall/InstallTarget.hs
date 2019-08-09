{-# LANGUAGE FlexibleInstances #-}
module Distribution.Client.CmdInstall.InstallTarget 
  ( InstallTarget(..)
  , ParsedTarget(..)
  , ResolvedTarget(..)
  , ParsedInstallTarget, ResolvedInstallTarget 
  ) where

import Distribution.Package
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.ComponentName
  ( ComponentName )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName )
import Distribution.Types.PackageId
  ( PackageIdentifier )
import Distribution.Client.TargetSelector
  ( ComponentKind(..), showComponentKindFilterShort )

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp

import Control.Applicative

data InstallTarget comp = InstallTarget
  { itPkgId :: PackageIdentifier
  , itComp :: comp
  } deriving (Show, Eq)

data ParsedTarget
  = Unqual UnqualComponentName
  | Qual ComponentName
  | Filter ComponentKind
  deriving (Show, Eq)

data ResolvedTarget
  = Local ComponentName
  | Repo ComponentName
  deriving (Show, Eq)

type ParsedInstallTarget = InstallTarget (Maybe ParsedTarget)
type ResolvedInstallTarget = InstallTarget ResolvedTarget

instance Pretty ParsedTarget where
  pretty (Unqual comp) = pretty comp
  pretty (Qual comp)   = pretty comp
  pretty (Filter filt) = Disp.text (showComponentKindFilterShort filt)

instance Pretty ResolvedTarget where
  pretty (Local comp) = pretty comp
  pretty (Repo comp)  = pretty comp
  
instance (Pretty comp) => Pretty (InstallTarget comp) where
  pretty (InstallTarget pkg mComp) = pretty pkg Disp.<> prettyComp mComp
    where
      prettyComp = (Disp.text ":" Disp.<>) . pretty  

instance Parsec ParsedTarget where
  parsec = P.try (Qual <$> parsec) 
       <|> Filter <$> filterParser
       <|> Unqual <$> parsec 
    where
      filterP f = (f <$) . P.choice . fmap P.string
      filterParser = (P.<?> "filter") . P.choice $  
        [ filterP LibKind liblabels
        , filterP FLibKind fliblabels
        , filterP ExeKind exelabels
        , filterP TestKind testlabels
        , filterP BenchKind benchlabels
        ]

      liblabels   = ["libs", "libraries"]
      fliblabels  = ["flibs", "foreign-libraries"]
      exelabels   = ["exes", "executables"]
      testlabels  = ["tests", "test-suites"]
      benchlabels = ["benches", "benchmarks"]

instance Parsec (InstallTarget (Maybe ParsedTarget)) where
  parsec = InstallTarget <$> parsec <*> optional (P.char ':' *> parsec)

instance Package (InstallTarget a) where
  packageId (InstallTarget pkgId _) = pkgId
