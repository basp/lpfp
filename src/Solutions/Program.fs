open System
open System.Security.Cryptography
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type Md5VsSha256 () =
    let N = 10_000
    let data = Array.zeroCreate<byte> N
    let sha256 = SHA256.Create()
    let md5 = MD5.Create()
    
    do
        let rnd = Random()
        rnd.NextBytes(data)
    
    [<Benchmark>]
    member _.Sha256() = sha256.ComputeHash(data)
    
    [<Benchmark>]
    member _.Md5() = md5.ComputeHash(data)

module Program =
    let [<EntryPoint>] main _ =
        let _ = BenchmarkRunner.Run<Md5VsSha256>()
        0
