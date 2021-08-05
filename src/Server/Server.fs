module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open System

open Shared

type Storage() =
    let todos = ResizeArray<_>()

    member __.GetTodos() = List.ofSeq todos

    member __.AddTodo(todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

    member __.CompleteTodo(id: Guid) =
        match todos |> Seq.tryFind (fun td -> td.Id = id) with
        | Some found -> todos.Remove found |> ignore
        | None -> ()

let storage = Storage()

storage.AddTodo(Todo.create "Create new SAFE project")
|> ignore

storage.AddTodo(Todo.create "Write your app")
|> ignore

storage.AddTodo(Todo.create "Ship it !!!")
|> ignore

let todosApi =
  { getTodos = fun () -> async { return storage.GetTodos() }
    addTodo =
      fun todo ->
        async {
          match storage.AddTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        }
    completeTodo = fun id -> async { storage.CompleteTodo id }
  }

let webApp =
  Remoting.createApi ()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.fromValue todosApi
  |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app