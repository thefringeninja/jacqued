namespace Jacqued

open System
open System.IO
open Android.App
open Android.Content
open Android.Content.PM
open Android.Views
open Avalonia
open Avalonia.Android
open Microsoft.Data.Sqlite
open SqlStreamStore

#nowarn "3261"

[<Activity(Theme = "@style/MyTheme.NoActionBar",
           WindowSoftInputMode = SoftInput.AdjustResize,
           MainLauncher = true,
           ConfigurationChanges = (ConfigChanges.Orientation ||| ConfigChanges.ScreenSize ||| ConfigChanges.UiMode),
#if JACQUED_DEV
           Label = "Jacqued (dev)",
           Icon = "@mipmap/ic_launcher",
           RoundIcon = "@mipmap/ic_launcher_round"
#else
           Label = "Jacqued",
           Icon = "@mipmap/ic_launcher",
           RoundIcon = "@mipmap/ic_launcher_round"
#endif
  )>]
[<IntentFilter([| Intent.ActionView |],
               Categories = [| Intent.CategoryDefault; Intent.CategoryOpenable |],
               DataScheme = "file",
               DataMimeType = "text/csv",
               DataHost = "*",
               DataPathPattern = ".*\\csv")>]
[<IntentFilter([| Intent.ActionCreateDocument |],
               Categories = [| Intent.CategoryDefault |],
               DataScheme = "file",
               DataMimeType = "text/csv",
               DataHost = "*",
               DataPathPattern = ".*\\csv")>]

type MainActivity() =
    inherit AvaloniaMainActivity()

    let dataSourceDirectory =
        Android.App.Application.Context.GetExternalFilesDir(null).Path

    let streamStore = MainActivity.createStreamStore dataSourceDirectory
    let settingsFile = FileInfo(Path.Combine(dataSourceDirectory, "settings.json"))

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
        AppBuilder
            .Configure<App>(fun () -> App(streamStore, settingsFile))
            .UseAndroid()

    override this.Dispose(disposing) =
        streamStore.Dispose()
        base.Dispose(disposing)
