module FameBoy.Web.Joypad

open Browser
open Browser.Types
open FameBoy.Joypad

module private Helpers =
    type JoypadButton =
        | Up
        | Down
        | Left
        | Right
        | A
        | B
        | Start
        | Select

    let buttonById =
        Map.ofList
            [ "up-button", Up
              "down-button", Down
              "left-button", Left
              "right-button", Right
              "a-button", A
              "b-button", B
              "start-button", Start
              "select-button", Select ]

    let buttonByKeyCode =
        Map.ofList
            [ "KeyW", Up
              "KeyS", Down
              "KeyA", Left
              "KeyD", Right
              "KeyK", A
              "KeyJ", B
              "KeyN", Start
              "KeyB", Select ]

    let p2ButtonByKeyCode =
        Map.ofList
            [ "ArrowUp", Up
              "ArrowDown", Down
              "ArrowLeft", Left
              "ArrowRight", Right
              "Home", A
              "PageUp", B
              "End", Start
              "PageDown", Select ]

open Helpers

let initJoypad () =
    let mutable pressed: Set<JoypadButton> = Set.empty

    // Fallback: if pointer/touch ends outside buttons, clear pressed state.
    window.addEventListener ("pointerup", fun _ -> pressed <- Set.empty)
    window.addEventListener ("pointercancel", fun _ -> pressed <- Set.empty)
    window.addEventListener ("blur", fun _ -> pressed <- Set.empty)

    window.addEventListener (
        "keydown",
        fun ev ->
            let code = (ev :?> KeyboardEvent).code

            match Map.tryFind code buttonByKeyCode with
            | Some b -> pressed <- pressed.Add b
            | None -> ()
    )

    window.addEventListener (
        "keyup",
        fun ev ->
            let code = (ev :?> KeyboardEvent).code

            match Map.tryFind code buttonByKeyCode with
            | Some b -> pressed <- pressed.Remove b
            | None -> ()
    )

    buttonById
    |> Map.iter (fun id button ->
        let el = document.getElementById id

        el.addEventListener (
            "pointerdown",
            fun e ->
                e.preventDefault ()
                pressed <- pressed.Add button
        ))

    fun () ->
        { Up = pressed.Contains Up
          Down = pressed.Contains Down
          Left = pressed.Contains Left
          Right = pressed.Contains Right
          A = pressed.Contains A
          B = pressed.Contains B
          Start = pressed.Contains Start
          Select = pressed.Contains Select }

let initJoypadP2 () =
    let mutable pressed: Set<JoypadButton> = Set.empty

    window.addEventListener ("blur", fun _ -> pressed <- Set.empty)

    window.addEventListener (
        "keydown",
        fun ev ->
            let ke = ev :?> KeyboardEvent
            let code = ke.code

            match Map.tryFind code p2ButtonByKeyCode with
            | Some b ->
                ev.preventDefault ()
                pressed <- pressed.Add b
            | None -> ()
    )

    window.addEventListener (
        "keyup",
        fun ev ->
            let code = (ev :?> KeyboardEvent).code

            match Map.tryFind code p2ButtonByKeyCode with
            | Some b -> pressed <- pressed.Remove b
            | None -> ()
    )

    fun () ->
        { Up = pressed.Contains Up
          Down = pressed.Contains Down
          Left = pressed.Contains Left
          Right = pressed.Contains Right
          A = pressed.Contains A
          B = pressed.Contains B
          Start = pressed.Contains Start
          Select = pressed.Contains Select }
