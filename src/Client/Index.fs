module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Browser

type Model =
  { Todos: Todo list
    Input: string
    Title: string option
    Error: string option }

type Msg =
  | GotTodos of Todo list
  | SetInput of string
  | TryAddTodo
  | AddedTodo of Todo

let todosApi =
  Remoting.createApi ()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    { Todos = []; Input = ""; Title = None; Error = None },
    Cmd.OfAsync.perform todosApi.getTodos () GotTodos

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value; Error = Todo.getError value }, Cmd.none
    | TryAddTodo ->
        let add todo = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        match Todo.tryCreate model.Input with
          | Ok todo -> { model with Input = ""; Error = None }, add todo
          | Error error -> { model with Error = Some error }, Cmd.none

    | AddedTodo todo ->
      { model with
          Todos = model.Todos @ [ todo ]
          Title = Some $"There are %i{model.Todos |> List.length |> (+) 1}"},
      Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
      Bulma.navbarItem.a [
        prop.href "https://safe-stack.github.io/"
        navbarItem.isActive
        prop.children [
          Html.img [
            prop.src "/favicon.png"
            prop.alt "Logo"
          ]
        ]
      ]
    ]

let todoInput (model: Model) (dispatch: Msg -> unit) =
    [
      Bulma.input.text [
        prop.value model.Input
        prop.placeholder "What needs to be done?"
        prop.onChange (fun x -> SetInput x |> dispatch)
        prop.onKeyPress (fun evt -> if evt.key = "Enter" then dispatch TryAddTodo else ())
        match model.Error with
          | Some _ -> color.isDanger
          | None -> if model.Input.Length > 0 then color.isSuccess
      ]
      Bulma.help [
        match model.Error with
          | Some _ -> color.isDanger
          | None -> color.isSuccess
        prop.text (model.Error |> Option.defaultValue "")
      ]
    ]

let listItems (todos: Todo list) : seq<Fable.React.ReactElement> =
    query { for todo in todos do select (Html.li [ prop.text todo.Description ]) }

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
      Bulma.content [ Html.ol (Seq.map (fun todo -> Html.li [ prop.text todo.Description ]) model.Todos) ]
      Bulma.field.div [
        field.isGrouped
        prop.children [
          Bulma.control.div [
            control.isExpanded
            prop.children (todoInput model dispatch)
          ]
          Bulma.control.p [
            Bulma.button.a [
              color.isPrimary
              prop.onClick (fun _ -> dispatch TryAddTodo)
              prop.text "Add"
            ]
          ]
        ]
      ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    document.title <- model.Title |> Option.defaultValue "TODO app"
    Bulma.hero [
      hero.isFullHeight
      color.isPrimary
      prop.style [
        style.backgroundSize "cover"
        style.backgroundImageUrl "https://unsplash.it/1200/900?random"
        style.backgroundPosition "no-repeat center center fixed"
      ]
      prop.children [
        Bulma.heroHead [
          Bulma.navbar [
            Bulma.container [ navBrand ]
          ]
        ]
        Bulma.heroBody [
          Bulma.container [
            Bulma.column [
              column.is6
              column.isOffset3
              prop.children [
                Bulma.title [
                  text.hasTextCentered
                  prop.text "TODO App to try the SAFE stack"
                ]
                containerBox model dispatch
              ]
            ]
          ]
        ]
      ]
    ]