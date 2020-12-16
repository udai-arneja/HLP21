module Renderer

    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    open Elmish
    open Elmish.React

    

    type Coords =
        {
            X : float
            Y : float
        }


    type Circle =
        {
            X : float
            Y : float
            Coords : Coords
            IsDragging : bool
        }


    type Model = Circle list


    type Msg =
        | StartDragging of index : int * pageX : float * pageY : float
        | Dragging of index : int * pageX : float * pageY : float
        | EndDragging of index : int


    let init () =
        [
            { 
                X = 50.
                Y = 50. 
                Coords =
                    {
                        X = 0.
                        Y = 0.
                }
                IsDragging = false
            }
            { 
                X = 150.
                Y = 150. 
                Coords =
                    {
                        X = 0.
                        Y = 0.
                }
                IsDragging = false
            }
        ]
        , Cmd.none


    let update (msg : Msg) (currentModel : Model)  =
        match msg with
        | StartDragging (rank, pageX, pageY) ->
            currentModel
            |> List.mapi (fun index circle ->
                if rank <> index then
                    circle
                else
                    { circle with
                        Coords =
                            {
                                X = pageX
                                Y = pageY
                            }
                        IsDragging = true
                    }
            )
            , Cmd.none

        | Dragging (rank, pageX, pageY) ->
            currentModel
            |> List.mapi (fun index circle ->
                if rank <> index then
                    circle
                else
                    let xDiff = circle.Coords.X - pageX
                    let yDiff = circle.Coords.Y - pageY
                    { circle with
                        X = circle.X - xDiff
                        Y = circle.Y - yDiff
                        Coords =
                            {
                                X = pageX
                                Y = pageY
                            }
                    }
            )
            , Cmd.none
    
        | EndDragging rank ->
            currentModel
            |> List.mapi (fun index circle ->
                if rank <> index then 
                    circle
                else
                    { circle with
                        IsDragging = false 
                    }
            )
            , Cmd.none


    type RenderCircleProps =
        {
            Circle : Circle
            Index : int
            Dispatch : Dispatch<Msg>
            key : string
        }


    let renderCircle =
        FunctionComponent.Of(
            fun (props : RenderCircleProps) ->
                let handleMouseMove =
                    Hooks.useRef(fun (ev : Types.Event) ->
                        let ev = ev :?> Types.MouseEvent

                        Dragging (props.Index, ev.pageX, ev.pageY)
                        |> props.Dispatch
                    )

                let color =
                    if props.Circle.IsDragging then
                        "lightblue"
                    else
                        "grey"

                circle
                    [ 
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Index
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            StartDragging (props.Index, ev.pageX, ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        Cx props.Circle.X
                        Cy props.Circle.Y
                        R 25.
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
        , "Circle"
        , equalsButFunctions
        )


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


    // App
    Program.mkProgram init update view
    |> Program.withReactSynchronous "app"
    |> Program.run

