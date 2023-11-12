port module Ports exposing (renderDot)


port renderDot : { engine : String, dotSource : String } -> Cmd msg
