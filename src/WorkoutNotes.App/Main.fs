module Main

open Elmish
open Elmish.React

Program.mkProgram App.init App.update App.view
|> Program.withReactSynchronous "root"
|> Program.run
