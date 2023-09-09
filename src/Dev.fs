module Dev

open Browser.Dom
open Browser.WebSocket

let private init _ =
    let ws = WebSocket.Create $"ws://%s{window.location.host}/websocket"
    ws.onmessage <- fun _ -> window.location.reload ()

window.addEventListener ("load", init)
