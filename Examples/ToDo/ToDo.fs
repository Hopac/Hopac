// Copyright (C) by Vesa Karvonen

module ToDo

// NOTES: This is not meant to demonstrate the most concise or best way to
// program with WPF and choice streams.  This example avoids data binding and
// some builtin WPF controls and overuses choice streams to show some more
// choice stream usage patterns.  WPF is a fundamentally imperative GUI
// framework and this example does not try to rewrite WPF as a more declarative
// framework.  Some of the simple techniques used in this example, such as using
// an immutable map as the model and removing and adding all controls to a panel
// when the model changes, do not necessarily scale.  Would have used XAML type
// provider, but it currently does not mix with PCL.
//
// The main problem with this example is that this doesn't really contain
// anything where choice streams work significantly better than Rx, such as
// asynchronous http requests, polling, streams requiring backpressure, or
// initialization of streams that would require hacks like `Publish-Connect` in
// Rx.  Although it doesn't really make any difference in this example, the
// `keepPreceding1` combinator is an example of something that is not supported
// by .Net Rx.  See comment below.
//
// See also: https://github.com/tastejs/todomvc/blob/master/app-spec.md

open Hopac
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
  /// Sets up the main synchronization context that we need for accessing GUI
  /// components.  Something like this should be put into a library.
  let setupWhen (onEvent: IEvent<_, _>) =
    let mainCtx = IVar ()
    let handler =
      EventHandler(fun _ _ ->
      IVar.tryFill mainCtx SynchronizationContext.Current |> start)
    onEvent.AddHandler handler
    mainCtx >>- fun ctx ->
    Hopac.Core.StaticData.writeLine <- Action<string>(MessageBox.Show >> ignore)
    Async.setMain ctx
    onEvent.RemoveHandler handler

  let inline (^) x = x

/// Represents an immutable ToDo item.
type Item = {
    IsCompleted: bool
    Header: string
  }

/// Module for generating unique ids.
module Id =
  let mutable private id = 0L

  /// Returns the next unique id.
  let next () = Interlocked.Increment &id

[<STAThread; EntryPoint>]
let main _ =
  /// The main window.
  let main = ToDo.UI.MainWindow ()

  // This makes it so that the stream wiring code is run after the main
  // synchronization context has been created.
  setupWhen main.Activated >>- fun () ->

      /// Serialized variable that holds the immutable model.  In this example
      /// it doesn't really make any difference, but the use of a serialized
      /// stream variable makes it easy to make sure that updates to the
      /// variable appear as atomic.
      let modelMVar = Stream.MVar.create Map.empty

      /// Functionally updates a ToDo item in the given model.
      let updateItem id f model =
        match Map.tryFind id model with
         | None -> model
         | Some item -> Map.add id (f item) model

      // This adds new ToDo items in response to Enter key presses on the
      // EnterHeader text box.
      Stream.ofObservableOnMain main.EnterHeader.KeyUp
      |> Stream.filterFun ^ fun evt -> evt.Key = Key.Enter
      |> Stream.chooseJob ^ fun _ -> onMain {
           return match main.EnterHeader.Text.Trim () with
                   | "" -> None
                   | header ->
                     main.EnterHeader.Text <- ""
                     Some header
         }
      |> Stream.consumeJob ^ fun header ->
           Stream.MVar.updateFun modelMVar ^
             Map.add (Id.next ()) {IsCompleted = false; Header = header}

      // Filtering predicates on ToDo items.
      let isAny _ = true
      let isCompleted item = item.IsCompleted
      let isActive item = item.IsCompleted |> not

      // This stream holds the chosen ToDo item filter.  This stream is
      // constructed so that it starts with one initial element and then
      // subsequent elements are generated in response to GUI filter button
      // clicks.  This stream also updates the GUI as a side-effect.
      let filter =
        let filterOn (button: Button) pred =
          Stream.ofObservableOnMain button.Click
          |> Stream.mapConst (button, pred)
        Stream.one (main.FilterAll, isAny)
        |> Stream.merge ^ filterOn main.FilterAll isAny
        |> Stream.merge ^ filterOn main.FilterActive isActive
        |> Stream.merge ^ filterOn main.FilterCompleted isCompleted
        |> Stream.keepPreceding1
        |> Stream.mapJob ^ fun (bn, pred) -> onMain {
             [main.FilterAll; main.FilterActive; main.FilterCompleted]
             |> Seq.iter ^ fun bn' -> bn'.IsEnabled <- bn <> bn'
             return pred
           }

      /// Creates a control for a ToDo item.
      let newControl id item =
        let control = ToDo.UI.ItemControl ()

        // These statements copy the values from the item to the controls.
        control.IsCompleted.IsChecked <- Nullable<bool> item.IsCompleted
        control.Header.Text <- item.Header
        control.Header.FontStyle <-
          if item.IsCompleted then FontStyles.Italic else FontStyles.Normal

        // This updates the underlying item in response to a click on the
        // completed checkbox.
        Stream.ofObservableOnMain control.IsCompleted.Click
        |> Stream.consumeJob ^ fun _ -> onMain {
             let isCompleted = control.IsCompleted.IsChecked.Value
             return! Stream.MVar.updateFun modelMVar ^
                       updateItem id ^ fun item ->
                         {item with IsCompleted = isCompleted}
           }

        // This enables editing the header of a ToDo item.
        Stream.ofObservableOnMain control.Header.MouseDoubleClick
        |> Stream.consumeJob ^ fun _ -> onMain {
             control.Header.Focusable <- true
             if control.Header.Focus () then
               control.Header.SelectAll ()
           }

        // This finalizes the editing of a ToDo item header after Enter key is
        // pressed or after focus is lost.  If the header would become empty
        // after trimming, the edit is ignored.
        Stream.ofObservableOnMain control.Header.KeyUp
        |> Stream.filterFun ^ fun evt -> evt.Key = Key.Enter
        |> Stream.mapIgnore
        |> Stream.merge (Stream.ofObservableOnMain control.Header.LostFocus
                         |> Stream.mapIgnore)
        |> Stream.consumeJob ^ fun () -> onMain {
             Keyboard.ClearFocus ()
             control.Header.Focusable <- false
             match control.Header.Text.Trim () with
               | "" -> control.Header.Text <- item.Header
               | header ->
                 return! Stream.MVar.updateFun modelMVar ^
                           updateItem id ^ fun item ->
                             {item with Header = header}
           }

        // This shows the Remove button of a ToDo item when the mouse is over
        // the item.  The stream is carefully constructed to avoid problems due
        // to race conditions; choice streams are asynchronous and there is no
        // absolute guarantee that elements from the MouseEnter stream would
        // always precede elements from the MouseLeave stream.
        Stream.merge
          <| Stream.mapConst +1 ^ Stream.ofObservableOnMain control.MouseEnter
          <| Stream.mapConst -1 ^ Stream.ofObservableOnMain control.MouseLeave
        |> Stream.scanFromFun 0 (+)
        |> Stream.keepPreceding1
        |> Stream.consumeJob ^ fun n -> onMain {
             control.Remove.Visibility <-
               if 0 < n then Visibility.Visible else Visibility.Hidden
           }

        // This removes the item from the model if the Remove button is clicked.
        Stream.ofObservableOnMain control.Remove.Click
        |> Stream.consumeJob ^ fun _ ->
             Stream.MVar.updateFun modelMVar ^ Map.remove id

        control

      // This renders the visible list of ToDo items.  All controls are
      // recreated.  Consider the use of `keepPreceding1`.  In this example it
      // doesn't really make any difference, but if it would be possible to make
      // updates to the model faster than it would be possible to render the
      // GUI, then it would basically avoid queuing GUI updates and make sure
      // that, once finally updated, the GUI would show the latest state.
      // Contrast this with Rx style throttling using some arbitrary millisecond
      // timeout.
      Stream.MVar.tap modelMVar
      |> Stream.combineLatest filter
      |> Stream.keepPreceding1
      |> Stream.consumeJob ^ fun (filter, model) -> onMain {
           main.Items.Children.Clear ()
           model
           |> Map.iter ^ fun id item ->
                if filter item then
                  newControl id item
                  |> main.Items.Children.Add
                  |> ignore
         }

      // This updates the display of the number of active ToDo items.
      Stream.MVar.tap modelMVar
      |> Stream.keepPreceding1
      |> Stream.consumeJob ^ fun model -> onMain {
           let n = Map.toSeq model |> Seq.filter (snd >> isActive) |> Seq.length
           main.NumberOfItems.Text <-
             sprintf "%d item%s left" n ^ if n=1 then "" else "s"
         }

      // This updates the button to clear all completed items.
      Stream.MVar.tap modelMVar
      |> Stream.keepPreceding1
      |> Stream.consumeJob ^ fun model -> onMain {
           let n = Map.toSeq model |> Seq.filter (snd >> isCompleted) |> Seq.length
           main.ClearCompleted.Visibility <-
             if n=0 then Visibility.Hidden else Visibility.Visible
         }

      // This removes all completed items when the ClearCompleted button is
      // clicked.
      Stream.ofObservableOnMain main.ClearCompleted.Click
      |> Stream.consumeJob ^ fun _ ->
           Stream.MVar.updateFun modelMVar ^ Map.filter ^ fun _ -> isActive

      // This updates the CompleteAll checkbox for marking all ToDo items as
      // completed or active.
      Stream.MVar.tap modelMVar
      |> Stream.mapFun ^ fun model ->
           not ^ Map.isEmpty model && model |> Map.forall ^ fun _ -> isCompleted
      |> Stream.keepPreceding1
      |> Stream.consumeJob ^ fun allCompleted -> onMain {
           main.CompleteAll.IsChecked <- Nullable<bool> allCompleted
         }

      // This makes all ToDo items as either completed or active depending on
      // the state of the CompleteAll checkbox when it is clicked.
      Stream.ofObservableOnMain main.CompleteAll.Click
      |> Stream.consumeJob ^ fun _ ->
           onMain { return main.CompleteAll.IsChecked.Value } >>= fun isCompleted ->
           Stream.MVar.updateFun modelMVar ^ Map.map ^ fun _ item ->
             {item with IsCompleted = isCompleted}

  |> queue

  Application().Run main
