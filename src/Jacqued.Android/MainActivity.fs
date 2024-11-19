namespace Jacqued

open System
open System.IO
open Android.App
open Android.Content
open Android.Content.PM
open Android.OS
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

    do this.streamStore <- MainActivity.createStreamStore Android.App.Application.Context

    static member createStreamStore(context: Context) =       
        let dataSource = Path.Combine(context.GetExternalFilesDir(null).Path, "jacqued.db")

        let cs = SqliteConnectionStringBuilder()
        cs.DataSource <- dataSource

        let settings = SqliteStreamStoreSettings(cs.ToString())
        settings.GetUtcNow <- (fun () -> DateTime.UtcNow)

        let store = new SqliteStreamStore(settings)
        store.CreateSchemaIfNotExists()
        store

    override _.CreateAppBuilder() =
        AppBuilder.Configure<App>(fun () -> App(this.streamStore)).UseAndroid()

    override this.Dispose(disposing) =
        this.streamStore.Dispose()
        base.Dispose(disposing)
