namespace FsMyers

type Difference<'a> = Insertion of int * 'a | Deletion of int
type IMyersList<'a> = abstract member Count : int
                      abstract member Item : int -> 'a with get

type Myers() =
    member __.GetDiffs(a, b) : Async<Difference<_> seq> =
        async { return Seq.empty }
    static member ApplyDifferences(insert, delete, differences, state) = state
    static member val Default = Myers() with get
