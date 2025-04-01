namespace Jacqued

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Styling
open DialogHostAvalonia
open Elmish
open Material.Colors
open Material.Icons.Avalonia
open Material.Styles.Themes
open SqlStreamStore

type MainWindow() =
    inherit HostWindow()

    do
        base.Title <- nameof Jacqued
        base.Height <- 915.0
        base.Width <- 412.0
#if DEBUG
        base.AttachDevTools(KeyGesture(Key.F12))
#endif

#nowarn "3261"

type App(store: IStreamStore, settingsPath) =
    inherit Application()

    override this.Initialize() =
        let theme = new MaterialTheme(null)        
        theme.PrimaryColor <- PrimaryColor.Red
        theme.SecondaryColor <- SecondaryColor.Red
        this.Styles.Add(theme)
        this.Styles.Add(MaterialIconStyles(null))
        this.Styles.Add(DialogHostStyles())        

    override this.OnFrameworkInitializationCompleted() =
        let host =
            match this.ApplicationLifetime with
            | :? ISingleViewApplicationLifetime as lifetime ->
                let control = HostControl()
                
                lifetime.MainView <- control
                (control :> IViewHost) |> Some
            | :? IClassicDesktopStyleApplicationLifetime as lifetime ->
                let main = MainWindow()

                lifetime.MainWindow <- main
                (main :> IViewHost) |> Some
            | _ -> None

        let settings = Configuration.load settingsPath
        match host with
        | Some hostControl ->
            let subscription _ : Sub<Msg> =
                let onActualThemeChanged dispatch =
                    this.PropertyChanged.Subscribe(fun e ->
                        if e.Property = Application.ActualThemeVariantProperty then
                            e.GetNewValue<ThemeVariant>() |> (Msg.ActualThemeSelected >> dispatch))
                [
                    [ nameof onActualThemeChanged ], onActualThemeChanged
                ]                
                    
            this.RequestedThemeVariant <- ThemeVariant.Default
            Program.mkProgram (Shell.init store settings) (Shell.update store) Shell.view
            |> Program.withHost hostControl
            |> Program.withSubscription subscription
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.runWithAvaloniaSyncDispatch ()
        | _ -> ()
