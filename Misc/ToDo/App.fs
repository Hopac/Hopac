// Copyright (C) by Vesa Karvonen

module ToDo

// NOTES: This is not meant to demonstrate the most concise or best way to
// program with WPF and choice streams.  This example avoids data binding and
// some builtin WPF controls and overuses choice streams to show some more
// choice stream usage patterns.  WPF is fundamentally an imperative UI
// framework and this example does not try to rewrite WPF as a more functional
// framework.  Some of the simple techniques used in this example, such using an
// immutable list as the model and removing and adding all controls to a panel
// when the model changes, do not scale (yes, there are builtin components for
// this in WPF).  Consider those as exercises to fix.  Would have used XAML type
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
    IsCompleted: Stream.Var<bool>
    Header: Stream.Var<string>
  }

[<STAThread; EntryPoint>]
let main argv =
  let main = ToDo.UI.MainWindow ()

  setupWhen main.Activated |>> fun () ->

      let itemsVar = Stream.Var.create []

      let items = Stream.Var.tap itemsVar

      let changedCompleted =
        items
        |> Stream.switchMap (fun items ->
           Stream.ofSeq items
           |> Stream.mergeMap (fun i -> Stream.Var.tap i.IsCompleted)
           |> Stream.mapConst items)
        |> Stream.merge items

      let itemsAndCtrls =
        items
        |> Stream.scanFromJob [] (fun oldItemsAndCtrls newItems -> onMain {
           let newCtrl item =
             let ctrl = ToDo.UI.ItemControl ()

             ctrl.Header.Text <- Stream.Var.get item.Header
             Stream.ofObservableOnMain ctrl.Header.MouseDoubleClick
             |> Stream.subscribeJob (fun _ -> onMain {
                ctrl.Header.Focusable <- true
                if ctrl.Header.Focus () then
                  ctrl.Header.SelectAll () })
             Stream.ofObservableOnMain ctrl.Header.KeyUp
             |> Stream.filterFun (fun e -> e.Key = Key.Enter)
             |> Stream.mapConst ()
             |> Stream.merge (Stream.ofObservableOnMain ctrl.LostFocus
                              |> Stream.mapConst ())
             |> Stream.subscribeJob (fun _ -> onMain {
                match ctrl.Header.Text.Trim () with
                 | "" ->
                   ctrl.Header.Text <- Stream.Var.get item.Header
                   ctrl.Header.Focusable <- false
                 | text ->
                   Keyboard.ClearFocus ()
                   ctrl.Header.Text <- text
                   ctrl.Header.Focusable <- false
                   return! Stream.Var.set item.Header text })

             Stream.ofObservableOnMain ctrl.IsCompleted.Click
             |> Stream.subscribeJob (fun _ -> onMain {
                let isChecked = ctrl.IsCompleted.IsChecked.Value
                if isChecked <> Stream.Var.get item.IsCompleted then
                  Stream.Var.set item.IsCompleted isChecked |> start })
             Stream.Var.tap item.IsCompleted
             |> Stream.subscribeJob (fun _ -> onMain {
                let isCompleted = Stream.Var.get item.IsCompleted
                if ctrl.IsCompleted.IsChecked.Value <> isCompleted then
                  ctrl.IsCompleted.IsChecked <- Nullable<bool> isCompleted })

             Stream.Var.tap item.IsCompleted
             |> Stream.subscribeJob (fun isCompleted -> onMain {
                ctrl.Header.FontStyle <-
                  if isCompleted then FontStyles.Italic else FontStyles.Normal })

             Stream.ofObservableOnMain ctrl.MouseEnter
             |> Stream.subscribeJob (fun _ -> onMain {
                ctrl.Remove.Visibility <- Visibility.Visible })
             Stream.ofObservableOnMain ctrl.MouseLeave
             |> Stream.subscribeJob (fun _ -> onMain {
                ctrl.Remove.Visibility <- Visibility.Hidden })

             Stream.ofObservableOnMain ctrl.Remove.Click
             |> Stream.subscribeJob (fun _ ->
                Stream.Var.get itemsVar
                |> List.filter (fun item' -> item' <> item)
                |> Stream.Var.set itemsVar)

             ctrl

           let itemToCtrl = Dictionary<_, _> ()
           oldItemsAndCtrls |> List.iter (fun (item, ctrl) -> itemToCtrl.[item] <- ctrl)

           return newItems
                  |> List.map (fun item ->
                     (item,
                      match itemToCtrl.TryGetValue item with
                       | (false, _) -> newCtrl item
                       | (_, ctrl) -> ctrl)) })

      let isAny _ = true
      let isCompleted it = Stream.Var.get it.IsCompleted
      let isActive it = isCompleted it |> not

      let filter =
        let filterOn (button: Button) pred =
          Stream.ofObservableOnMain button.Click
          |> Stream.mapConst (button, pred)
        Stream.one (main.FilterAll, isAny)
        |> Stream.merge (filterOn main.FilterAll isAny)
        |> Stream.merge (filterOn main.FilterActive isActive)
        |> Stream.merge (filterOn main.FilterCompleted isCompleted)
        |> Stream.mapJob (fun (bn, pred) -> onMain {
           [main.FilterAll; main.FilterActive; main.FilterCompleted]
           |> Seq.iter (fun bn' -> bn'.IsEnabled <- bn <> bn')
           return pred })

      let filteredItemsAndCtrls =
        changedCompleted
        |> Stream.combineLatest itemsAndCtrls
        |> Stream.combineLatest filter
        |> Stream.mapFun (fun (filter, (ics, _)) ->
           ics
           |> List.filter (fst >> filter))

      filteredItemsAndCtrls
      |> Stream.subscribeJob (fun fics -> onMain {
         main.Items.Children.Clear ()
         fics
         |> Seq.iter (snd >> main.Items.Children.Add >> ignore) })

      Stream.ofObservableOnMain main.EnterHeader.KeyUp
      |> Stream.filterFun (fun e -> e.Key = Key.Enter)
      |> Stream.chooseJob (fun _ -> onMain {
         return match main.EnterHeader.Text.Trim () with
                 | "" -> None
                 | header -> Some header })
      |> Stream.subscribeJob (fun header ->
         onMain { main.EnterHeader.Text <- "" } >>.
         let item = {IsCompleted = Stream.Var.create false
                     Header = Stream.Var.create header}
         (Stream.Var.get itemsVar @ [item])
         |> Stream.Var.set itemsVar)

      changedCompleted
      |> Stream.subscribeJob (fun items -> onMain {
         let n = items |> Seq.filter isActive |> Seq.length
         main.NumberOfItems.Text <- sprintf "%d items left" n })

      changedCompleted
      |> Stream.subscribeJob (fun items -> onMain {
         let n = items |> Seq.filter isCompleted |> Seq.length
         main.ClearCompleted.Visibility <-
           if n=0 then Visibility.Hidden else Visibility.Visible
         main.ClearCompleted.Content <- sprintf "Clear completed %d" n})

      Stream.ofObservableOnMain main.ClearCompleted.Click
      |> Stream.subscribeJob (fun _ ->
         Stream.Var.get itemsVar
         |> List.filter (fun item -> Stream.Var.get item.IsCompleted |> not)
         |> Stream.Var.set itemsVar)

      changedCompleted
      |> Stream.mapFun (fun items ->
         items.Length <> 0 && List.forall isCompleted items)
      |> Stream.subscribeJob (fun allCompleted -> onMain {
         main.CompleteAll.IsChecked <- Nullable<bool> allCompleted })

      Stream.ofObservableOnMain main.CompleteAll.Click
      |> Stream.subscribeJob (fun _ ->
         onMain { return main.CompleteAll.IsChecked.Value } >>= fun completed ->
         Stream.Var.get itemsVar
         |> Seq.iterJob (fun item -> Stream.Var.set item.IsCompleted completed))

  |> queue

  Application().Run main
