// Copyright (C) by Vesa Karvonen

module ToDo

// NOTES: This is not meant to demonstrate the most concise or best way to
// program with WPF and choice streams.  This example avoids data binding and
// some builtin WPF controls and overuses choice streams to show some more
// choice stream usage patterns.  WPF is fundamentally an imperative UI
// framework and this example does not try to rewrite WPF as a more declarative
// framework.  Some of the simple techniques used in this example, such using an
// immutable map as the model and removing and adding all controls to a panel
// when the model changes, do not necessarily scale.  Would have used XAML type
// provider, but it currently does not mix with PCL.
//
// See also: https://github.com/tastejs/todomvc/blob/master/app-spec.md

open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Infixes
open Hopac.Extensions

open System
open System.Collections.Generic
open System.Threading
open System.Windows
open System.Windows.Controls
open System.Windows.Input

[<AutoOpen>]
module internal Util =
  let setupWhen (onEvent: IEvent<_, _>) =
    let mainCtx = ivar ()
    let handler =
      EventHandler(fun _ _ ->
      IVar.tryFill mainCtx SynchronizationContext.Current |> start)
    onEvent.AddHandler handler
    mainCtx |>> fun ctx ->
    Hopac.Core.StaticData.writeLine <- Action<string>(MessageBox.Show >> ignore)
    Async.setMain ctx
    onEvent.RemoveHandler handler

type Item = {
    IsCompleted: bool
    Header: string
  }

module Id =
  let mutable private id = 0L
  let next () = Interlocked.Increment &id

[<STAThread; EntryPoint>]
let main argv =
  let main = ToDo.UI.MainWindow ()

  setupWhen main.Activated |>> fun () ->

      let modelMVar = Stream.MVar.create Map.empty

      let updateItem id f model =
        match Map.tryFind id model with
         | None -> model
         | Some item -> Map.add id (f item) model

      Stream.ofObservableOnMain main.EnterHeader.KeyUp
      |> Stream.filterFun (fun evt -> evt.Key = Key.Enter)
      |> Stream.chooseJob (fun _ -> onMain {
         return match main.EnterHeader.Text.Trim () with
                 | "" -> None
                 | header ->
                   main.EnterHeader.Text <- ""
                   Some header })
      |> Stream.subscribeJob (fun header ->
         Stream.MVar.updateFun modelMVar
          <| Map.add (Id.next ()) {IsCompleted = false; Header = header})

      let isAny _ = true
      let isCompleted item = item.IsCompleted
      let isActive item = item.IsCompleted |> not

      let filter =
        let filterOn (button: Button) pred =
          Stream.ofObservableOnMain button.Click
          |> Stream.mapConst (button, pred)
        Stream.one (main.FilterAll, isAny)
        |> Stream.merge (filterOn main.FilterAll isAny)
        |> Stream.merge (filterOn main.FilterActive isActive)
        |> Stream.merge (filterOn main.FilterCompleted isCompleted)
        |> Stream.keepPreceding1
        |> Stream.mapJob (fun (bn, pred) -> onMain {
           [main.FilterAll; main.FilterActive; main.FilterCompleted]
           |> Seq.iter (fun bn' -> bn'.IsEnabled <- bn <> bn')
           return pred })

      let newControl id item =
        let control = ToDo.UI.ItemControl ()

        control.IsCompleted.IsChecked <- Nullable<bool> item.IsCompleted
        control.Header.Text <- item.Header
        control.Header.FontStyle <-
          if item.IsCompleted then FontStyles.Italic else FontStyles.Normal

        Stream.ofObservableOnMain control.IsCompleted.Click
        |> Stream.subscribeJob (fun _ -> onMain {
           let isCompleted = control.IsCompleted.IsChecked.Value
           return! Stream.MVar.updateFun modelMVar
                     << updateItem id <| fun item ->
                         {item with IsCompleted = isCompleted} })

        Stream.ofObservableOnMain control.Header.MouseDoubleClick
        |> Stream.subscribeJob (fun _ -> onMain {
           control.Header.Focusable <- true
           if control.Header.Focus () then
             control.Header.SelectAll () })
        Stream.ofObservableOnMain control.Header.KeyUp
        |> Stream.filterFun (fun evt -> evt.Key = Key.Enter)
        |> Stream.mapIgnore
        |> Stream.merge (Stream.ofObservableOnMain control.Header.LostFocus
                        |> Stream.mapIgnore)
        |> Stream.subscribeJob (fun () -> onMain {
           Keyboard.ClearFocus ()
           control.Header.Focusable <- false
           match control.Header.Text.Trim () with
             | "" -> control.Header.Text <- item.Header
             | header ->
               return! Stream.MVar.updateFun modelMVar << updateItem id <| fun item ->
                       {item with Header = header} })

        Stream.merge
          (Stream.ofObservableOnMain control.MouseEnter |> Stream.mapConst +1)
          (Stream.ofObservableOnMain control.MouseLeave |> Stream.mapConst -1)
        |> Stream.scanFromFun 0 (+)
        |> Stream.keepPreceding1
        |> Stream.subscribeJob (fun n -> onMain {
           control.Remove.Visibility <-
             if 0 < n then Visibility.Visible else Visibility.Hidden })

        Stream.ofObservableOnMain control.Remove.Click
        |> Stream.subscribeJob (fun _ ->
           Stream.MVar.updateFun modelMVar <| Map.remove id)

        control

      Stream.MVar.tap modelMVar
      |> Stream.combineLatest filter
      |> Stream.keepPreceding1
      |> Stream.subscribeJob (fun (filter, model) -> onMain {
         main.Items.Children.Clear ()
         model
         |> Map.iter (fun id item ->
            if filter item then
              newControl id item
              |> main.Items.Children.Add
              |> ignore) })

      Stream.MVar.tap modelMVar
      |> Stream.keepPreceding1
      |> Stream.subscribeJob (fun model -> onMain {
         let n = Map.toSeq model |> Seq.filter (snd >> isActive) |> Seq.length
         main.NumberOfItems.Text <- sprintf "%d items left" n })

      Stream.MVar.tap modelMVar
      |> Stream.keepPreceding1
      |> Stream.subscribeJob (fun model -> onMain {
         let n = Map.toSeq model |> Seq.filter (snd >> isCompleted) |> Seq.length
         main.ClearCompleted.Visibility <-
           if n=0 then Visibility.Hidden else Visibility.Visible
         main.ClearCompleted.Content <- sprintf "Clear completed %d" n })

      Stream.ofObservableOnMain main.ClearCompleted.Click
      |> Stream.subscribeJob (fun _ ->
         Stream.MVar.updateFun modelMVar << Map.filter <| fun _ -> isActive)

      Stream.MVar.tap modelMVar
      |> Stream.mapFun (fun model ->
         not (Map.isEmpty model) && Map.forall (fun _ -> isCompleted) model)
      |> Stream.keepPreceding1
      |> Stream.subscribeJob (fun allCompleted -> onMain {
         main.CompleteAll.IsChecked <- Nullable<bool> allCompleted })

      Stream.ofObservableOnMain main.CompleteAll.Click
      |> Stream.subscribeJob (fun _ ->
         onMain { return main.CompleteAll.IsChecked.Value } >>= fun isCompleted ->
         Stream.MVar.updateFun modelMVar << Map.map <| fun _ item ->
         {item with IsCompleted = isCompleted})

  |> queue

  Application().Run main
