module UI.Button(Type(Nothing), message) where

import Signal

type Type = Nothing

message : Signal.Mailbox Type
message = Signal.mailbox Nothing
