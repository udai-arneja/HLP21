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
    // Multi: bool;
    Boxes: (XYPos * XYPos * CommonTypes.ComponentId) list;
    Selected: (CommonTypes.ComponentId) list;
    Hovering: CommonTypes.ComponentId list;
    LastOp: Helpers.MouseOp;
    SCursor: XYPos;             // start cursor for drawing wire or multi-select box
    ECursor: XYPos;             // end cursor for drawing wire or multi-select box
    WorMS: bool;                 //true is multi-select box, false is wire
    SelectedPort: CommonTypes.Port list
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

//selection,hovering and unselection colors - these can be updated to interface with symbol or buswire
let selColorMsg (symId: CommonTypes.ComponentId list) : Msg = (Wire <| BusWire.Symbol (Symbol.SymbolColor (symId,CommonTypes.Red)) )
let hovColorMsg (symId: CommonTypes.ComponentId list) : Msg = (Wire <| BusWire.Symbol (Symbol.SymbolColor (symId,CommonTypes.Green)) )
let unSelColorMsg (symId: CommonTypes.ComponentId list) : Msg  = (Wire <| BusWire.Symbol (Symbol.SymbolColor (symId,CommonTypes.Grey)) )
let newWire (ports: CommonTypes.Port * CommonTypes.Port) : Msg = (Wire <| BusWire.AddWire ports )

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom: float) (svgReact: ReactElement) (mSW:XYPos) (mSH:XYPos) (sLine:XYPos) (eLine:XYPos) (worMS:bool) (dispatch: Dispatch<Msg>)=
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

                    //if wire between ports draw a line, otherwise draw a multi-selecting box
                    if worMS
                    then
                        polygon [
                            SVGAttr.Points dimensions
                            SVGAttr.StrokeWidth "1px"
                            SVGAttr.Stroke "Black"
                            SVGAttr.FillOpacity 0.1
                            SVGAttr.Fill "Blue"
                            ] []
                    else
                        line [
                            X1 sLine.X; Y1 sLine.Y; X2 eLine.X; Y2 eLine.Y; Style [Stroke "Black";StrokeWidth "3px"]
                        ] []
                ]
            ]
        ]


let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let zoom = model.Zoom
    let startMSBox= model.SCursor
    let endMSBox=model.ECursor
    let startline = model.SCursor
    let endline = model.ECursor
    let worMS = model.WorMS
    displaySvgWithZoom zoom wireSvg startMSBox endMSBox startline endline worMS dispatch
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    
    //model helper functions
    
    //update the bounding boxes of the boxes in boxesToUpdate
    //removes current certain bounding boxes, and appends updated bounding boxes
    let updateBBox (boxesToUpdate:(XYPos*XYPos*CommonTypes.ComponentId) list) : (XYPos*XYPos*CommonTypes.ComponentId) list =
        let updateIndiv (pos1,pos2,iD)=
            let newSymPos= (List.find (fun sym -> sym.Id=iD) model.Wire.Symbol.SymbolsList).Pos
            let posDiff= {X=newSymPos.X-pos1.X;Y=newSymPos.Y-pos1.Y}
            ({X = newSymPos.X; Y = newSymPos.Y},{X=pos2.X+posDiff.X; Y=pos2.Y+posDiff.Y},iD)
        model.Boxes
        |> List.filter (fun x -> (not (List.contains x boxesToUpdate)))
        |> List.append (List.map updateIndiv boxesToUpdate)

    //checks if components are in the start and end corners of the mutlti-select box
    let inSelBox ((sc:XYPos), (ec:XYPos)): (CommonTypes.ComponentId) list=     //sc : start corner, ec: end corner
        let corners = if sc.X < ec.X     //dragging left to right
                         then if sc.Y < ec.Y
                              then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
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
        List.choose overlap model.Boxes
    
    //checks to see if the mousepress position is in range of any of the ports on the component
    let inRangeofPort (mousepos:XYPos) (sId:CommonTypes.ComponentId) : Option<CommonTypes.Port> = 
        let symb : Symbol = List.find (fun sym -> sym.Id=sId) model.Wire.Symbol.SymbolsList
        let dist (p1:Helpers.XYPos) (p2:XYPos) = sqrt((p1.X-p2.X)**2. + (p1.Y-p2.Y)**2.)
        match List.tryFind (fun (prt:CommonTypes.Port) -> (dist prt.PortPos mousepos)<40. ) symb.InputPorts with
        | Some x -> Some x
        | None -> match List.tryFind (fun (prt:CommonTypes.Port) -> (dist prt.PortPos mousepos)<40. ) symb.OutputPorts with
                  | Some x -> Some x
                  | None -> None
        
    //message and model processing and updating
    match msg with
    //input from the mouse:
    | Wire (BusWire.MouseMsg {Op = mouseState ; Pos = { X = mX; Y = mY}; Zoom = zoom}) ->

        //return component Id if mouse is over a component
        let overComp = List.tryFind (fun (co1,co2,symId) -> co1.X<mX && co2.X>mX && co1.Y<mY && co2.Y>mY) model.Boxes

        //mouseposition - used when dragging to make the multi-select box
        let mousePos = {X=mX;Y=mY}
        
        match mouseState with
        //DOWN: checks if mousepressed over a component or not:
        // 1 - over a component -> if no components are already selected the component or its port are selected
                                  //if components are selected and this component is among them, nothing is done
                                  // otherwise all already selected component are unselected and the new component is selected
        // 2 - not over a component -> unselected any selected componnents and starts drawing a multi-select box
        | Down -> match overComp with
                  | Some (pos1,pos2,iD) -> if List.isEmpty model.Selected
                                           then match (inRangeofPort mousePos iD) with
                                                | Some startport -> if List.isEmpty model.Hovering
                                                                    then {model with LastOp=Down;SCursor=startport.PortPos;ECursor=startport.PortPos;WorMS=false;SelectedPort=[startport]}, Cmd.none
                                                                    else {model with LastOp=Down;SCursor=startport.PortPos;ECursor=startport.PortPos;WorMS=false;SelectedPort=[startport];Hovering=[]}, Cmd.ofMsg (unSelColorMsg [iD])
                                                | None -> {model with Selected=[iD];LastOp=Down}, Cmd.ofMsg (selColorMsg [iD])
                                            else if List.contains iD model.Selected
                                                 then {model with LastOp=Down}, Cmd.none
                                                 else {model with Selected=[iD];LastOp=Down}, Cmd.ofMsg (unSelColorMsg model.Selected) 
                  | None -> if model.Selected <> []
                            then let iD = model.Selected.[0]
                                 {model with Selected=[];LastOp=Down;SCursor=mousePos;ECursor=mousePos;WorMS=true},Cmd.ofMsg (unSelColorMsg [iD])
                            else {model with LastOp=Down;SCursor=mousePos;ECursor=mousePos;WorMS=true}, Cmd.none
        //UP :if the last mouse operation was drag then the type of drag needs to be determined, there are three cases
        // 1- drawing a selecting box in which case any components in the box need be selected (func- inSelBox)
        // 2 - drawing a wire in which case the ports needs to be validated and sent to BusWire to make a new comp.
        // 3 - moving selected components - in which case the bounded boxes for all moved components need to be updated
        | Up -> match model.LastOp with
                | Drag -> if List.isEmpty model.Selected
                          then if model.WorMS
                                    //multi-select box
                               then let multiselected= inSelBox (model.SCursor,model.ECursor)
                                    {model with Selected=multiselected;LastOp=Up;ECursor={X=0.;Y=0.};SCursor={X=0.;Y=0.}}, Cmd.ofMsg (selColorMsg multiselected)
                               else match overComp with
                                    //wire
                                    | Some (p1,p2,iD) -> match (inRangeofPort mousePos iD) with
                                                         | Some endport -> let ports = (model.SelectedPort.[0],endport)
                                                                           {model with LastOp=Up;ECursor={X=0.;Y=0.};SCursor={X=0.;Y=0.};SelectedPort=[]}, Cmd.ofMsg (newWire ports)
                                                         | None -> {model with LastOp=Drag}, Cmd.none
                                    | None -> {model with LastOp=Up;ECursor={X=0.;Y=0.};SCursor={X=0.;Y=0.};SelectedPort=[]}, Cmd.none
                          else let updateBBoxes = List.filter (fun (pos1,pos2,symid) -> (List.exists (fun x -> x=symid) model.Selected) ) model.Boxes
                               {model with Selected=[];Boxes=(updateBBox updateBBoxes);LastOp=Up}, Cmd.ofMsg (unSelColorMsg model.Selected)
                | _ -> {model with LastOp=Up}, Cmd.none
        //DRAG: if no components are selected, either a multi-select box or a wire is being drawn in which case its current
        //end point components need to be updated
        | Drag -> if List.isEmpty model.Selected
                  then {model with LastOp=Drag;ECursor=mousePos}, Cmd.none
                  else {model with LastOp=Drag}, Cmd.none
        //MOVE: if the cursor is over a components and no other components are selected then a hovering message is sent
        // at the moment this simply updates the colour but will ultimately be up to symbol to decide what is changed
        // to indicate hovering. Once no longer hovering over a component, the component will return to normal state
        | Move -> match overComp with
                  | Some (pos1,pos2,iD) -> if List.isEmpty model.Selected
                                           then {model with Hovering=[iD]}, Cmd.ofMsg (hovColorMsg [iD])
                                           else model, Cmd.none
                  | None -> if List.isEmpty model.Hovering
                            then model, Cmd.none
                            else if List.isEmpty model.Selected 
                                 then let iD = model.Hovering.[0]
                                      {model with Hovering=[]},Cmd.ofMsg (unSelColorMsg [iD])   
                                 else {model with Hovering=[]},Cmd.none

    // sending messages to buswire - only comm. path
    // all other update functions that need to comm. with other paths\
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

    //new component
    | KeyPress AltN ->
        let compid = CommonTypes.ComponentId (Helpers.uuid())
        let symbolPos = {X=200.;Y=200.}
            //can be mouse co-ordinates or however the position of a new component will be implemented
        let newCompInfo = (symbolPos,compid)
            //all the info needed for a new comp - updated when interfacing with Symbol
        let boundingBoxInfo = {X=symbolPos.X;Y=symbolPos.Y},{X=symbolPos.X+300.;Y=symbolPos.Y+500.},compid
            //creating a bounding box - either calc. or use component type dimensions?
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

//  FOR INTERFACING

    //returns a list of wires to be deleted
    // let delWires (symbList:Symbol.Symbol list) : BusWire.Wire list = 
    //     List.filter (fun wire -> (List.contains wire.SrcSymbol symbList) || (List.contains wire.TargetSymbol symbList)) model.Wire.WX

let init() = 
    let model,cmds = (BusWire.init 0)()
    {
        Wire = model
        Zoom = 1.0
        // Multi = false
        Boxes=[]
        Selected=[]
        Hovering=[]
        LastOp = Move
        SCursor={X=0.;Y=0.}
        ECursor={X=0.;Y=0.}
        WorMS=true
        SelectedPort=[]
    }, Cmd.map Wire cmds



