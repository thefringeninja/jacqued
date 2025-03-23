namespace Jacqued

open System
open System.IO
open Android.App
open Android.Content.PM
open Avalonia
open Avalonia.Android
open Microsoft.Data.Sqlite
open SqlStreamStore

#nowarn "3261"

[<Activity(Label = "Jacqued.Android",
           Theme = "@style/MyTheme.NoActionBar",
           // Icon = "@drawable/icon",
           MainLauncher = true,
           ConfigurationChanges = (ConfigChanges.Orientation ||| ConfigChanges.ScreenSize ||| ConfigChanges.UiMode))>]
type MainActivity() as this =
    inherit AvaloniaMainActivity()

    [<DefaultValue(false)>]
    val mutable streamStore: IStreamStore

    [<DefaultValue(false)>]
    val mutable settingsPath: string

    do
        let dataSourceDirectory = Android.App.Application.Context.GetExternalFilesDir(null).Path
        this.streamStore <- MainActivity.createStreamStore dataSourceDirectory
        this.settingsPath <- Path.Combine(dataSourceDirectory, "settings.json")

    static member createStreamStore(dataSourceDirectory) =        
        let dataSource = Path.Combine(dataSourceDirectory, "jacqued.db")

        let cs = SqliteConnectionStringBuilder()
        cs.DataSource <- dataSource

        let settings = SqliteStreamStoreSettings(cs.ToString())
        settings.GetUtcNow <- (fun () -> DateTime.UtcNow)

        let store = new SqliteStreamStore(settings)
        store.CreateSchemaIfNotExists()
        store

    override _.CreateAppBuilder() =
        AppBuilder.Configure<App>(fun () -> App(this.streamStore, this.settingsPath)).UseAndroid()

    override this.Dispose(disposing) =
        this.streamStore.Dispose()
        base.Dispose(disposing)
