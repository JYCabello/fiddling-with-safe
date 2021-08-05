module Index

open Elmish
open Fable.Remoting.Client
open Feliz.propModule
open Shared
open Browser
open System

type Model =
  { Todos: Todo list
    Input: string
    Error: string option
    Completing: Guid list }
  member self.Title: string option =
    self.Todos
    |> List.length
    |> (fun count -> if count = 0 then None else Some $"There are %i{count} items to do")


type Msg =
  | GotTodos of Todo list
  | SetInput of string
  | TryAddTodo
  | AddedTodo of Todo
  | CompleteTodo of Todo
  | CompletedTodo of Guid

let todosApi =
  Remoting.createApi ()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    { Todos = []; Input = ""; Error = None; Completing = [] },
    Cmd.OfAsync.perform todosApi.getTodos () GotTodos

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value; Error = Todo.getError value }, Cmd.none
    | TryAddTodo ->
        let add todo =
          Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        match Todo.tryCreate model.Input with
        | Ok todo -> { model with Input = ""; Error = None }, add todo
        | Error error -> { model with Error = Some error }, Cmd.none

    | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    | CompleteTodo todo ->
      { model with Completing = model.Completing @ [ todo.Id ] },
      Cmd.OfAsync.perform todosApi.completeTodo todo.Id CompletedTodo
    | CompletedTodo id ->
      { model with
          Todos = model.Todos |> List.filter (fun t -> t.Id = id |> not)
          Completing = model.Completing |> List.filter (fun c -> c = id |> not) },
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

let isCompleting (model: Model) (todo: Todo) : bool =
  model.Completing |> List.exists (fun id -> id = todo.Id)

let todoInput (model: Model) (dispatch: Msg -> unit) =
  Components.validatedInput
    model.Input
    model.Error
    (SetInput >> dispatch)
    (Some <| fun () -> (dispatch TryAddTodo))
    (Some "What needs to be done?")

let todoItem (model: Model) (todo: Todo) (dispatch: Msg -> unit) =
  let completion =
      if not <| isCompleting model todo
      then Components.okButton "Complete" (fun _ -> dispatch <| CompleteTodo todo)
      else Html.span [ prop.text "Completing..." ]

  Html.li [
    prop.children [
      Html.span [
        prop.text todo.Description
      ]
      completion
    ]
  ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
      Bulma.content [
        Html.ol [
          for todo in model.Todos do todoItem model todo dispatch
        ]
      ]
      Bulma.field.div [
        field.isGrouped
        prop.children [
          Bulma.control.div [
            control.isExpanded
            prop.children (todoInput model dispatch)
          ]
          Components.okButton "Add" (fun _ -> dispatch TryAddTodo)
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