// Copyright (C) by Vesa Karvonen

module ToDo

// NOTES: This is not meant to demonstrate the most concise or best way to
// program with WPF and choice streams.  This example overuses choice streams to
// show some more usage patterns.  WPF is fundamentally an imperative UI
// framework and this example does not try to rewrite WPF as a more functional
// framework.  Some of the simple techniques in this example, such using an
// immutable list as the model and removing and adding all controls when the
// model changes, do not scale.  Consider those as exercises to fix.

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
    IsCompleted: Stream.Var<bool>
    Header: string
  }

[<STAThread; EntryPoint>]
let main argv =
  let main = ToDo.UI.MainWindow ()

  setupWhen main.Activated |>> fun () ->

      let itemsVar = Stream.Var.create []

      let itemsAndCtrls =
        Stream.Var.tap itemsVar
        |> Stream.scanFromJob [] (fun oldItemsAndCtrls newItems -> onMain {
           let newCtrl item =
             let ctrl = ToDo.UI.ItemControl ()

             ctrl.Header.Text <- item.Header

             Stream.ofObservableOnMain ctrl.IsCompleted.Click
             |> Stream.iterJob (fun _ -> onMain {
                let isChecked = ctrl.IsCompleted.IsChecked.Value
                if isChecked <> Stream.Var.get item.IsCompleted then
                  Stream.Var.set item.IsCompleted isChecked |> start })
             |> queue
             Stream.Var.tap item.IsCompleted
             |> Stream.iterJob (fun _ -> onMain {
                let isCompleted = Stream.Var.get item.IsCompleted
                if ctrl.IsCompleted.IsChecked.Value <> isCompleted then
                  ctrl.IsCompleted.IsChecked <- Nullable<bool> isCompleted })
             |> queue

             Stream.Var.tap item.IsCompleted
             |> Stream.iterJob (fun isCompleted -> onMain {
                ctrl.Header.FontStyle <-
                  if isCompleted then FontStyles.Italic else FontStyles.Normal })
             |> queue

             Stream.ofObservableOnMain ctrl.Remove.Click
             |> Stream.iterJob (fun _ -> 
                Stream.Var.get itemsVar
                |> List.filter (fun item' -> item' <> item)
                |> Stream.Var.set itemsVar)
             |> queue

             ctrl

           let itemToCtrl = Dictionary<_, _> ()
           oldItemsAndCtrls |> List.iter (fun (item, ctrl) -> itemToCtrl.[item] <- ctrl)

           return newItems
                  |> List.map (fun item ->
                     (item,
                      match itemToCtrl.TryGetValue item with
                       | (false, _) -> newCtrl item
                       | (_, ctrl) -> ctrl)) })

      let filter =
        let filterOn (button: Button) pred =
          Stream.ofObservableOnMain button.Click
          |> Stream.mapConst (button, pred)
        Stream.one (main.FilterAll, fun _ -> true)
        |> Stream.merge (filterOn main.FilterAll (fun _ -> true))
        |> Stream.merge (filterOn main.FilterActive (fun it -> Stream.Var.get it.IsCompleted |> not))
        |> Stream.merge (filterOn main.FilterCompleted (fun it -> Stream.Var.get it.IsCompleted))
        |> Stream.mapJob (fun (bn, pred) -> onMain {
           [main.FilterAll; main.FilterActive; main.FilterCompleted]
           |> Seq.iter (fun bn' -> bn'.IsEnabled <- bn <> bn')
           return pred })

      let filteredItemsAndCtrls =
        itemsAndCtrls
        |> Stream.switchMap (fun ics ->
           ics
           |> Stream.ofSeq
           |> Stream.mergeMap (fun (i, _) -> Stream.Var.tap i.IsCompleted))
        |> Stream.merge (Stream.one false)
        |> Stream.combineLatest itemsAndCtrls
        |> Stream.combineLatest filter
        |> Stream.mapFun (fun (filter, (ics, _)) ->
           ics
           |> List.filter (fst >> filter))

      filteredItemsAndCtrls
      |> Stream.iterJob (fun fics -> onMain {
         main.Items.Children.Clear ()
         fics
         |> Seq.iter (snd >> main.Items.Children.Add >> ignore) })
      |> queue

      Stream.ofObservableOnMain main.EnterHeader.KeyUp
      |> Stream.chooseJob (fun e -> onMain {
         return match main.EnterHeader.Text with
                 | _ when e.Key <> Key.Enter -> None
                 | "" -> None
                 | header -> Some header })
      |> Stream.filterFun (fun header ->
         Stream.Var.get itemsVar
         |> List.forall (fun item -> item.Header <> header))
      |> Stream.iterJob (fun header -> onMain {
         let item = {IsCompleted = Stream.Var.create false; Header = header}
         main.EnterHeader.Text <- ""
         return! (Stream.Var.get itemsVar @ [item])
                 |> Stream.Var.set itemsVar })
      |> queue

      filteredItemsAndCtrls
      |> Stream.combineLatest itemsAndCtrls
      |> Stream.iterJob (fun (ics, fics) -> onMain {
         main.NumberOfItems.Text <-
           sprintf "%d/%d items"
             <| List.length fics
             <| List.length ics })
      |> queue

      Stream.ofObservableOnMain main.ClearCompleted.Click
      |> Stream.iterJob (fun _ ->
         Stream.Var.get itemsVar
         |> List.filter (fun item -> Stream.Var.get item.IsCompleted |> not)
         |> Stream.Var.set itemsVar)
      |> queue

  |> queue

  Application().Run main
