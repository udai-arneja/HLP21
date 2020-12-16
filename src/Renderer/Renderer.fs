module Renderer

    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    open Elmish
    open Elmish.React

    open Symbol




    // App
    Program.mkProgram Symbol.init Symbol.update Sheet.view
    |> Program.withReactSynchronous "app"
    |> Program.run

