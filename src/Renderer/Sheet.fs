﻿module Sheet

open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers


type Model = {
    Wire: BusWire.Model;
    Zoom: float;
    Multi: bool;
    Boxes: (XYPos*XYPos) list
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | AltUp | AltDown | Comd

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg

// type TopMenu = | Closed | Project | Files

///helper functions to be moved out later
 

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom: float) (multisel:bool) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX/zoom; Y = ev.clientY/zoom}; Zoom = zoom})
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels     
                    // AlignItems AlignItemsOptions.Center
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                [ 
                    text [ // a demo text svg element
                        X 500; 
                        Y 50; 
                        Style [
                            TextAnchor "middle" // horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // vertical alignment vs (X,Y)
                            FontSize "40px"
                            FontWeight "Bold"
                            Fill "Green" // font color
                        ]
                        ] [str "sample text"]

                    svgReact // the application code
                ]
            ]
        ]


/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let zoom = model.Zoom
    let multisel = model.Multi
    displaySvgWithZoom zoom multisel wireSvg dispatch
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress Comd ->
        printfn "Cmd"
        {model with Multi=true}, Cmd.none
    | KeyPress AltUp ->
        // let wModel, wCmd = 
        printfn "Zoom In"
        {model with Zoom=model.Zoom+0.1}, Cmd.none
    | KeyPress AltDown ->
        printfn "Zoom Out"
        {model with Zoom=model.Zoom-0.1}, Cmd.none
    | KeyPress DEL ->
        model, Cmd.none
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)

let init() = 
    let model,cmds = (BusWire.init 10)()
    {
        Wire = model
        Zoom = 1.0
        Multi = false
        Boxes=[]
    }, Cmd.map Wire cmds


