module Dev

open Browser.Dom
open Fable.Core

[<Emit("typeof EventSource !== 'undefined'")>]
let private hasEventSource: bool = jsNative

[<AllowNullLiteral>]
type private IEventSource =
    abstract addEventListener: string * (obj -> unit) -> unit
    abstract close: unit -> unit
    abstract onmessage: (obj -> unit) with get, set

[<Emit("new EventSource($0)")>]
let private createEventSource (_url: string) : IEventSource = jsNative

let private initLiveReloadingViaSse () =
    let es = createEventSource "/sse"

    let reload (_: obj) =
        es.close ()
        window.location.reload ()

    es.addEventListener ("refresh", reload)
    es.onmessage <- reload

    window.addEventListener ("beforeunload", (fun _ -> es.close ()))

let private initLiveReloading _ =
    // SSE only.
    // If the browser doesn't support EventSource, do nothing.
    if hasEventSource then
        initLiveReloadingViaSse ()

window.addEventListener ("load", initLiveReloading)
