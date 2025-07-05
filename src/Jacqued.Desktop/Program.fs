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

        dataSourceDirectory.Create()

        let settingsFile = FileInfo(Path.Combine(dataSourceDirectory.FullName, "settings.json"))

        use store =
            new SqliteStreamStore(
                SqliteStreamStoreSettings(
                    SqliteConnectionStringBuilder(DataSource = Path.Combine(dataSourceDirectory.FullName, "Jacqued.db"))
                        .ToString(),
                    GetUtcNow = (fun () -> DateTime.UtcNow)
                )
            )

        store.CreateSchemaIfNotExists()

        AppBuilder
            .Configure<App>(fun () -> App(store, settingsFile))
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
