module Tests

open Expecto

[<Tests>]
let tests =
    testSequenced
    <| testList "Integration testing" [
        ServerTests.tests
        SnapshotTests.tests
        BooklogTests.tests
        SearchTests.tests
        LlmTests.tests
    ]
