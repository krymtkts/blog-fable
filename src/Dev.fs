module Dev

open Browser.Dom
open Browser.WebSocket

let private initLiveReloading _ =
    // NOTE: don't use string interpolation here, it will break the code because of importing String module.
    let ws = WebSocket.Create <| "ws://" + window.location.host + "/websocket"

    ws.onmessage <-
        fun _ ->
            ws.close (1000, "reload")
            window.location.reload ()

    window.addEventListener ("beforeunload", (fun _ -> ws.close ()))

window.addEventListener ("load", initLiveReloading)
