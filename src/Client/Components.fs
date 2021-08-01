module Components

open Feliz
open Feliz.Bulma

let okButton (text: string) onClick =
  Bulma.control.p [
    Bulma.button.a [
      color.isPrimary
      prop.onClick onClick
      prop.text text
    ]
  ]

let validatedInput
  (value: string)
  (error: string option)
  (onChange: string -> unit)
  (onEnter: (unit -> unit) option)
  (placeholder: string option) =
    [
      Bulma.input.text [
        prop.value value
        if placeholder.IsSome then prop.placeholder placeholder.Value
        prop.onChange onChange
        if onEnter.IsSome then prop.onKeyPress (fun evt -> if evt.key = "Enter" then onEnter.Value() else ())
        match error with
          | Some _ -> color.isDanger
          | None -> if value.Length > 0 then color.isSuccess
      ]
      Bulma.help [
        if error.IsSome then color.isDanger
        if error.IsSome then prop.text error.Value
      ]
    ]