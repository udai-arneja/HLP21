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
    Boxes: (XYPos * XYPos * CommonTypes.ComponentId) list
    Selected: (CommonTypes.ComponentId) list
    Hovering: CommonTypes.ComponentId list
    LastOp: Helpers.MouseOp
    StartMultiSelBox: XYPos
    EndMultiSelBox: XYPos
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | AltUp | AltDown | CmdD | AltN

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg

type SelectingBox={
    TopCorner: XYPos
    BottomCorner: XYPos
}

// type TopMenu = | Closed | Project | Files

///helper functions to be moved out later

//selection,hovering and unselection colors
let selColorMsg symId= (Wire <| BusWire.Symbol (Symbol.SymbolColor (symId,CommonTypes.Red)) )
let hovColorMsg symId= (Wire <| BusWire.Symbol (Symbol.SymbolColor (symId,CommonTypes.Green)) )
let unSelColorMsg symId = (Wire <| BusWire.Symbol (Symbol.SymbolColor (symId,CommonTypes.Grey)) )

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom: float) (multisel:bool) (svgReact: ReactElement) mSW mSH (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1500. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    
    let dimensions = sprintf "%f,%f %f,%f %f,%f %f,%f" mSW.X mSW.Y mSW.X mSH.Y mSH.X mSH.Y mSH.X mSW.Y
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
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move)ev)
          OnMouseDown (fun ev -> (mouseOp Down ev))
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels     
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                [ 
                    text [ // a demo text svg element
                        X 750; 
                        Y 75; 
                        Style [
                            TextAnchor "middle" // horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // vertical alignment vs (X,Y)
                            FontSize "80px"
                            FontWeight "Bold"
                            Fill "Green" // font color
                        ]
                        ] [str "A whole lot of experimentation"]

                    svgReact // the application code

                    polygon [
                        SVGAttr.Points dimensions
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0.1
                        SVGAttr.Fill "Blue"
                        ] []
                ]
            ]
        ]


/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let zoom = model.Zoom
    let multisel = model.Multi
    let startMSBox= model.StartMultiSelBox
    let endMSBox= model.EndMultiSelBox
    displaySvgWithZoom zoom multisel wireSvg startMSBox endMSBox dispatch
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    
    let updateBBox (pos1,pos2,iD)=
        let newSymPos= (List.find (fun sym -> sym.Id=iD) model.Wire.Symbol.SymbolsList).Pos
        let posDiff= {X=newSymPos.X-pos1.X;Y=newSymPos.Y-pos1.Y}
        model.Boxes
        |> List.except [(pos1,pos2,iD)]
        |> List.append [({X = newSymPos.X; Y = newSymPos.Y},{X=pos2.X+posDiff.X; Y=pos2.Y+posDiff.Y},iD)]

    let inSelBox (sc, ec) =     //sc : start corner, ec: end corner
        let corners = if sc.X < ec.X     //dragging left to right
                         then if sc.Y < ec.Y
                              then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
                                // (sc, ec)
                              else {TopCorner={X=sc.X;Y=ec.Y};BottomCorner={X=ec.X;Y=sc.Y}}    //dragging down to up
                         else if sc.Y > ec.Y    //dragging right to left
                              then {TopCorner=ec;BottomCorner=sc}  //dragging down to up
                              else {TopCorner={X=ec.X;Y=sc.Y};BottomCorner={X=sc.X;Y=ec.Y}}   //dragging up to down
        
        let overlap (pos1,pos2,id) = if corners.TopCorner.X<pos1.X && corners.BottomCorner.X>pos1.X 
                                        ||corners.TopCorner.X<pos2.X && corners.BottomCorner.X>pos2.X
                                     then if corners.TopCorner.Y<pos1.Y && corners.BottomCorner.Y>pos1.Y
                                             ||corners.TopCorner.Y<pos2.Y && corners.BottomCorner.Y>pos2.Y
                                          then Some id
                                          else None
                                     else None
        List.choose (fun (pos1,pos2,id) -> overlap (pos1,pos2,id)) model.Boxes

    match msg with
    | Wire (BusWire.MouseMsg {Op = mouseState ; Pos = { X = mX; Y = mY}; Zoom = zoom}) ->
        //is this mouse over a component - if so return the component Id
        let overComp = List.tryFind (fun (co1,co2,symId) -> co1.X<mX && co2.X>mX && co1.Y<mY && co2.Y>mY) model.Boxes
        //mouseposition - used when dragging to make the multi-select box
        let mouseDownPos = {X=mX;Y=mY}
        
        match mouseState with
        | Down -> match overComp with
                  | Some (pos1,pos2,iD) -> printfn "Selected: %A" iD
                                           if List.isEmpty model.Selected
                                           then {model with Selected=[iD];LastOp=Down}, Cmd.ofMsg (selColorMsg [iD])
                                           else {model with Selected=[];LastOp=Down}, Cmd.ofMsg (unSelColorMsg model.Selected)  
                                           //need to unselect and select new!
                  | None -> if model.Selected <> []
                            then let iD = model.Selected.[0]
                                 {model with Selected=[];LastOp=Down;StartMultiSelBox=mouseDownPos;EndMultiSelBox=mouseDownPos},Cmd.ofMsg (unSelColorMsg [iD])
                            else {model with LastOp=Down;StartMultiSelBox=mouseDownPos;EndMultiSelBox=mouseDownPos}, Cmd.none
        | Up -> printfn "Last Op: %A" model.LastOp
                match model.LastOp with
                | Drag -> if List.isEmpty model.Selected
                          then let multiselected= inSelBox (model.StartMultiSelBox,model.EndMultiSelBox)
                               {model with Selected=multiselected;LastOp=Up;EndMultiSelBox={X=0.;Y=0.};StartMultiSelBox={X=0.;Y=0.}}, Cmd.ofMsg (selColorMsg multiselected) //implement multi-select by dragging
                          else printfn "Updated BBox"
                               let pos1,pos2,iD = List.find (fun (pos1,pos2,symid) -> symid=model.Selected.[0]) model.Boxes
                               {model with Selected=[];Boxes=updateBBox(pos1,pos2,iD);LastOp=Up}, Cmd.ofMsg (unSelColorMsg [iD])
                               //if the over comp is true then should be green rather than grey?
                | _ -> {model with LastOp=Up}, Cmd.none
        | Drag -> if List.isEmpty model.Selected
                  then {model with LastOp=Drag;EndMultiSelBox=mouseDownPos}, Cmd.none
                  else {model with LastOp=Drag}, Cmd.none
        | Move -> match overComp with
                  | Some (pos1,pos2,iD) -> printfn "Hovering: %A" iD
                                           if List.isEmpty model.Selected
                                           then {model with Hovering=[iD]}, Cmd.ofMsg (hovColorMsg [iD])
                                           else model, Cmd.none
                  | None -> if List.isEmpty model.Hovering
                            then model, Cmd.none    //hovering empty
                            else if List.isEmpty model.Selected 
                                 then let iD = model.Hovering.[0]   //hovering not empty, selected empty
                                      {model with Hovering=[]},Cmd.ofMsg (unSelColorMsg [iD])   
                                 else {model with Hovering=[]},Cmd.none //hovering not empty, selected not empty

    // sending messages to buswire - only comm. path
    // all other update functions that need to comm. with other paths
    // will send an update function to this DU
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats()
        model, Cmd.none

    //setting up for multi select - yet to implement properly
    // | KeyPress CmdD ->
    //     printfn "CmdD"
    //     {model with Multi=true}, Cmd.none

    | KeyPress AltN ->
        printfn "New Component"
        let symbolPos = {X=200.;Y=200.}
            //can be mouse co-ordinates or however the position of a new component will be implemented
        let compid = CommonTypes.ComponentId (Helpers.uuid())
        let newCompInfo = (symbolPos,compid)
            //all the info needed for a new comp - updated when interfacing with Symbol
        let boundingBoxInfo = {X=symbolPos.X;Y=symbolPos.Y},{X=symbolPos.X+300.;Y=symbolPos.Y+500.},compid
            //creating a bounding box - either calc. or use component type dimensions?
        printfn "%A" compid
        {model with Boxes=boundingBoxInfo::model.Boxes}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.AddSquare newCompInfo))
    
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
            List.filter (fun (pos1,pos2,bBoxId)-> match List.tryFind (fun delId-> delId=bBoxId) delCompList with
                                                  | Some x -> false
                                                  | None -> true) model.Boxes
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
        Boxes=[]
        Selected=[]
        Hovering=[]
        LastOp = Move
        StartMultiSelBox={X=0.;Y=0.}
        EndMultiSelBox={X=0.;Y=0.}
    }, Cmd.map Wire cmds
//need to remove all the initial boxes and do bounding boxes as components are added


