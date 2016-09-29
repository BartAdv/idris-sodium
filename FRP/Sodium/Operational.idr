module FRP.Sodium.Event

import FRP.Sodium.Event
import FRP.Sodium.Cell

%access export

updates : Cell a -> Event a
updates (MkCell e _) = e
