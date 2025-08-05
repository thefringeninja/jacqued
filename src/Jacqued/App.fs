namespace Jacqued

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Styling
open DialogHostAvalonia
open Elmish
open Jacqued.Data
open Jacqued.Msg
open Material.Colors.Recommended
open Material.Icons.Avalonia
open Material.Styles.Themes
open SqlStreamStore

type MainWindow() =
    inherit HostWindow(Title = nameof Jacqued, Height = 915.0, Width = 412.0)

#if DEBUG
    do base.AttachDevTools(KeyGesture(Key.F12))
#endif

#nowarn "3261"

type private JacqedTheme() as this =
    inherit CustomMaterialTheme(null)

    do
        this.Palettes.Add(
            ThemeVariant.Light,
            CustomMaterialThemeResources(PrimaryColor = RedSwatch.Red500, SecondaryColor = AmberSwatch.Amber500)
        )

        this.Palettes.Add(
            ThemeVariant.Dark,
            CustomMaterialThemeResources(PrimaryColor = RedSwatch.Red200, SecondaryColor = AmberSwatch.Amber200)
        )

type App(store: IStreamStore, settingsFile) =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(new JacqedTheme())
        this.Styles.Add(MaterialIconStyles(null))
        this.Styles.Add(DialogHostStyles())

    override this.OnFrameworkInitializationCompleted() =
        let host =
            match this.ApplicationLifetime with
            | :? ISingleViewApplicationLifetime as lifetime ->
                let control = HostControl()

                lifetime.MainView <- control
                ((control :> IViewHost), TopLevel.GetTopLevel(control).StorageProvider) |> Some
            | :? IClassicDesktopStyleApplicationLifetime as lifetime ->
                let main = MainWindow()

                lifetime.MainWindow <- main
                ((main :> IViewHost), main.StorageProvider) |> Some
            | _ -> None

        let settings = Configuration.load settingsFile

        match host with
        | Some(hostControl, storageProvider) ->
            let subscription _ : Sub<Msg> =
                let onActualThemeChanged dispatch =
                    this.PropertyChanged.Subscribe(fun e ->
                        if e.Property = Application.ActualThemeVariantProperty then
                            e.GetNewValue<ThemeVariant>()
                            |> (Settings.ActualThemeSelected >> Msg.Settings >> dispatch))

                [ [ nameof onActualThemeChanged ], onActualThemeChanged ]

            this.RequestedThemeVariant <- settings.ThemeVariant

            let backupManager = BackupManager(store, storageProvider, (fun () -> DateTime.Now))

            let init = Shell.init store settings
            let update = Shell.update store backupManager
            let view = Shell.view

            Program.mkProgram init update view
            |> Program.withHost hostControl
            |> Program.withSubscription subscription
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.runWithAvaloniaSyncDispatch ()
        | _ -> ()
