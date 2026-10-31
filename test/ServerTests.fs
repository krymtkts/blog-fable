module ServerTests

open Expecto
open Expecto.Flip
open DevServer
open System.Net
open System.Net.Sockets
open System.Net.Http
open TestHelpers

let private sequentialRestarts =
    testTask "Dev server serves requests after sequential restarts on the same port" {
        do!
            task {
                for attempt in 1..5 do
                    do!
                        task {
                            use! server = DevServer.StartAsync()
                            use client = new HttpClient()
                            let url = $"http://localhost:%d{server.Port}%s{server.Root}/index.html"
                            use! response = client.GetAsync(url)
                            let! content = response.Content.ReadAsStringAsync()

                            response.IsSuccessStatusCode
                            |> Expect.isTrue $"Dev server did not serve index.html after restart %d{attempt}"

                            content.Contains "<!DOCTYPE html>"
                            |> Expect.isTrue $"Dev server did not return HTML after restart %d{attempt}"
                        }

                    use listener =
                        new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)

                    listener.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true)

                    listener.Bind(IPEndPoint(IPAddress.Loopback, int port))
            }
    }

let tests = testList "Dev server" [ sequentialRestarts ]
