namespace Shared

open System

type Todo =
  { Id: Guid; Description: string }

module Todo =
  let isValid (description: string) =
    String.IsNullOrWhiteSpace description |> not

  let create (description: string) =
    { Id = Guid.NewGuid()
      Description = description }

  let getError description =
    if isValid description then None else Some "A todo item must have a value"

  let tryCreate (description: string) =
    match getError description with
      | None -> Ok { Id = Guid.NewGuid(); Description = description }
      | Some error -> Error error

module Route =
  let builder typeName methodName =
    $"/api/%s{typeName}/%s{methodName}"

type ITodosApi =
  { getTodos: unit -> Async<Todo list>
    addTodo: Todo -> Async<Todo> }