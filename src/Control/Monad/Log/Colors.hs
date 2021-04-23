{-# options_ghc -Wall #-}
module Control.Monad.Log.Colors where

import Control.Monad.Log
import Data.String
import System.Console.ANSI
import Data.Text.Prettyprint.Doc

-- | Apply 'SGR' codes to a string to modify its display attributes, resetting
-- SGR codes afterward.
wrapSGRCode :: (IsString a, Monoid a) => [SGR] -> a -> a
wrapSGRCode codes t = mconcat
  [ fromString $ setSGRCode codes
  , t
  , fromString $ setSGRCode [Reset]
  ]

-- | Mapping of 'Severity' levels to SGR styles
severitySgr :: Severity -> [SGR]
severitySgr = \case
  Emergency  ->
    [ SetColor Background Vivid Red
    , SetColor Foreground Vivid Black
    , SetConsoleIntensity BoldIntensity
    ]
  Alert ->
    [ SetColor Foreground Vivid Red
    , SetConsoleIntensity BoldIntensity
    ]
  Critical ->
    [ SetColor Foreground Vivid Red
    , SetConsoleIntensity BoldIntensity
    ]
  Error ->
    [ SetColor Foreground Dull Red
    ]
  Warning ->
    [ SetColor Foreground Vivid Yellow
    ]
  Notice ->
    [ SetColor Foreground Vivid Blue
    ]
  Informational -> []
  Debug -> 
    [ SetColor Foreground Dull Green
    ]

-- | Color based on severity with a custom mapping of severity to SGR styles
colorizeWith
  :: (Monoid msg, IsString msg)
  => (Severity -> [SGR])
  -> WithSeverity msg
  -> WithSeverity msg
colorizeWith f (WithSeverity sev msg) =
  WithSeverity sev $ wrapSGRCode (f sev) msg

-- | Color based on severity with the default mapping of severity to SGR styles
colorize 
  :: (Monoid msg, IsString msg)
  => WithSeverity msg
  -> WithSeverity msg
colorize = colorizeWith severitySgr

renderWithColor :: (Monoid msg, IsString msg, Pretty msg) => WithSeverity msg -> Doc ann
renderWithColor = renderWithSeverity pretty . colorize
