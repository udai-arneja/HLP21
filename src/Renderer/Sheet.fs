module Sheet

open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers
open Symbol


type Model = {
    Wire: BusWire.Model;
    Zoom: float;
    Multi: bool;
    // Boxes: (XYPos*XYPos) list
    Boxes: Map<(XYPos*XYPos), CommonTypes.ComponentId>
    Selected: (CommonTypes.ComponentId) list
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | AltUp | AltDown | CmdD | AltN

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
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move)ev)
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
                        ] [str "A lot of experimentation"]

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
    | Wire (BusWire.MouseMsg {Op = mouseState ; Pos = { X = mX; Y = mY}; Zoom = zoom}) ->
        //is this mouse over a component - if so return the component Id
        let overComp = match Map.tryFindKey (fun (co1,co2) symbolid -> co1.X<mX && co2.X>mX && co1.Y<mY && co2.Y>mY) model.Boxes with
                       | Some x -> Some model.Boxes.[x]
                       | None -> None
        //mouseState to determine functionality
        match mouseState with
        //click - 
        | Down -> match overComp with
                  | Some x -> printfn "Selected: %A" x
                              {model with Selected=x::model.Selected}, Cmd.none
                              //need to send list of selected items - for highlighting
                  | None -> {model with Selected=[]} , Cmd.none
        | Up -> model, Cmd.none
        | Drag -> match overComp with
                  | Some x -> printfn "%A" x
                              model, Cmd.none
                  | None -> model, Cmd.none
        | Move -> model, Cmd.none
    // sending messages to buswire - only comm. path
    // all other update functions that need to comm. with other paths
    // will send an update function to this DU
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    //setting up for multi select - yet to implement properly
    // | KeyPress CmdD ->
    //     printfn "CmdD"
    //     {model with Multi=true}, Cmd.none
    // creating a new symbol - just a circle for now
    // need to make coordinates come from mouse
    | KeyPress AltN ->
        printfn "New Component"
        let symbolPos = {X=200.;Y=200.}
            //can be mouse co-ordinates or however the position of a new component will be implemented
        let compid = CommonTypes.ComponentId (Helpers.uuid())
        let newCompInfo = (symbolPos,compid)
            //all the info needed for a new comp - updated when interfacing with Symbol
        let boundingBox = ({X=symbolPos.X-20.;Y=symbolPos.Y-20.},{X=symbolPos.X+20.;Y=symbolPos.Y+20.})
            //creating a bounding box - either calc. or use component type dimensions?
        printfn "%A" compid
        {model with Boxes=(Map.add boundingBox compid model.Boxes)}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.AddCircle newCompInfo))
    
    | KeyPress AltUp ->
        printfn "Zoom In"
        {model with Zoom=model.Zoom+0.1}, Cmd.none

    | KeyPress AltDown ->
        printfn "Zoom Out"
        {model with Zoom=model.Zoom-0.1}, Cmd.none

    | KeyPress DEL ->
        let delCompList = model.Selected
        //remove the bounding boxes
        let remainingbBoxes = 
            model.Boxes
            |> Map.toList
            // |> List.map (fun (x,y) -> (y,x))
            |> List.filter (fun (pos,bBoxId)-> match List.tryFind (fun delId-> delId=bBoxId) delCompList with
                                               | Some x -> false
                                               | None -> true) 
            |> Map.ofList
        {model with Selected=[];Boxes=remainingbBoxes},Cmd.ofMsg (Wire <| BusWire.Symbol (DeleteSymbol delCompList))

    // Wire Colour changes
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)

let init() = 
    let model,cmds = (BusWire.init 0)()
    {
        Wire = model
        Zoom = 1.0
        Multi = false
        Boxes=Map.empty
        Selected=[]
    }, Cmd.map Wire cmds
//need to remove all the initial boxes and do bounding boxes as components are added


