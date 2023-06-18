const init = () =>{
    websocket = new WebSocket("ws://"+window.location.host+"/websocket");
    websocket.onmessage = (e) => location.reload();
}
window.addEventListener("load", init, false);
