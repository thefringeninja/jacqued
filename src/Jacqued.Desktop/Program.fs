namespace Jacqued

open System
open System.IO
open Avalonia
open Microsoft.Data.Sqlite
open SqlStreamStore

module Program =

    [<EntryPoint>]
    let main (args: string[]) =
        let dataSourceDirectory =
            DirectoryInfo(Path.Combine((Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData), "Jacqued"))
        let settingsPath = Path.Combine(dataSourceDirectory.FullName, "settings.json")

        use store = 
            dataSourceDirectory.Create()

            let dataSource = Path.Combine(dataSourceDirectory.FullName, "Jacqued.db")

            let cs = SqliteConnectionStringBuilder()
            cs.DataSource <- dataSource

            let settings = SqliteStreamStoreSettings(cs.ToString())
            settings.GetUtcNow <- (fun () -> DateTime.UtcNow)
            let store = new SqliteStreamStore(settings)
            store.CreateSchemaIfNotExists()
            store

        AppBuilder
            .Configure<App>(fun () -> App(store, settingsPath))
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
