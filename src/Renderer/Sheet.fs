module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Symbol
open BusWire

let view (model : Model) (dispatch : Msg -> unit) =
    let handleSvgMouseEvent ev = ()
         
    let circles =
        model
        |> List.mapi (fun index circle ->
            renderCircle 
                {
                    Circle = circle
                    Index = index
                    Dispatch = dispatch
                    key = "circle-" + string index
                }
        )
        |> ofList
    
    svg [
            Style 
                [
                    Border "1px solid green"
                    Height "500px"
                    Width "calc(100% - 20px)"
                    Margin "10px"
                ]
        ]
        [ circles ]


