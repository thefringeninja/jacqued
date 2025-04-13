namespace Jacqued

open System
open System.IO
open Android.App
open Android.Content.PM
open Android.Views
open Avalonia
open Avalonia.Android
open Microsoft.Data.Sqlite
open SqlStreamStore

#nowarn "3261"

[<Activity(Label = "Jacqued.Android",
           Theme = "@style/MyTheme.NoActionBar",
           // Icon = "@drawable/icon",
           WindowSoftInputMode = SoftInput.AdjustResize,
           MainLauncher = true,
           ConfigurationChanges = (ConfigChanges.Orientation ||| ConfigChanges.ScreenSize ||| ConfigChanges.UiMode))>]
type MainActivity() =
    inherit AvaloniaMainActivity()

    let dataSourceDirectory =
        Android.App.Application.Context.GetExternalFilesDir(null).Path

    let streamStore = MainActivity.createStreamStore dataSourceDirectory
    let settingsPath = Path.Combine(dataSourceDirectory, "settings.json")

    static member createStreamStore(dataSourceDirectory) =
        let store =
            new SqliteStreamStore(
                SqliteStreamStoreSettings(
                    SqliteConnectionStringBuilder(DataSource = Path.Combine(dataSourceDirectory, "jacqued.db"))
                        .ToString(),
                    GetUtcNow = (fun () -> DateTime.UtcNow)
                )
            )

        store.CreateSchemaIfNotExists()
        store

    override _.CreateAppBuilder() =
        AppBuilder.Configure<App>(fun () -> App(streamStore, settingsPath)).UseAndroid()

    override this.Dispose(disposing) =
        streamStore.Dispose()
        base.Dispose(disposing)
