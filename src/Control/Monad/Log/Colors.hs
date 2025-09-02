{-# options_ghc -Wall #-}
module Control.Monad.Log.Colors
  ( wrapSGRCode
  , emergency
  , alert
  , critical
  , err
  , warning
  , notice
  , info
  , debug
  , trace
  , severitySgr
  , colorizeWith
  , colorize
  , renderWithColor
  , SGR(..)
  )
  where

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

emergency, alert, critical, err, warning, notice, info, debug, trace :: [SGR]
emergency =
  [ SetColor Background Vivid Red
  , SetColor Foreground Vivid Black
  , SetConsoleIntensity BoldIntensity
  ]
alert =
  [ SetColor Foreground Vivid Red
  , SetConsoleIntensity BoldIntensity
  ]
critical =
  [ SetColor Foreground Vivid Red
  , SetConsoleIntensity BoldIntensity
  ]
err =
  [ SetColor Foreground Dull Red
  ]
warning =
  [ SetColor Foreground Vivid Yellow
  ]
notice =
  [ SetColor Foreground Vivid Blue
  ]
info =
  []
debug =
  [ SetColor Foreground Dull Green
  ]
trace =
  [ SetColor Foreground Dull White
  ]

-- | Mapping of 'Severity' levels to SGR styles
severitySgr :: Severity -> [SGR]
severitySgr = \case
  Emergency -> emergency
  Alert -> alert
  Critical -> critical
  Error -> err
  Warning -> warning
  Notice -> notice
  Informational -> info
  Debug -> debug

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
