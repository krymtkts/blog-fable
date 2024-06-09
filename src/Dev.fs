module Dev

open Browser.Dom
open Browser.WebSocket

let private initLiveReloading _ =
    let ws = WebSocket.Create $"ws://%s{window.location.host}/websocket"

    ws.onmessage <-
        fun _ ->
            ws.close (1000, "reload")
            window.location.reload ()

    window.addEventListener ("unload", (fun _ -> ws.close ()))

window.addEventListener ("load", initLiveReloading)
