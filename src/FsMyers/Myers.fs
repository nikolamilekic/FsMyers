namespace FsMyers

open System.Threading

type Difference<'a> = Insertion of int * 'a | Deletion of int
type IMyersList<'a> = abstract member Count : int
                      abstract member Item : int -> 'a with get

module Myers =
    type SnakeDirection = private Forward | Reverse
    type SnakeOrientation = private Horizontal | Vertical
    type Region = private { AStart : int; N : int; BStart : int; M : int }
    type Snake = private | CompleteMatch
                         | Middle of Region * SnakeDirection * SnakeOrientation

    let isEven x = x % 2 = 0

    let getDifferences comparison (a : IMyersList<_>) (b : IMyersList<_>) = async {
        let vArrayLength = a.Count + b.Count + 1
        use buffers =
            new ThreadLocal<int[] * int[]>(
                fun _ -> Array.create vArrayLength 0,
                         Array.create vArrayLength 0)

        let getMiddleSnake { AStart = aStart; N = n; BStart = bStart; M = m } =
            let offset = m
            let delta = n - m
            let deltaIsEven = isEven delta
            let mIsEven = isEven m
            let nIsEven = isEven n
            let vForward, vReverse = buffers.Value

            vForward.[offset + 1] <- 0
            vReverse.[offset + delta - 1] <- n

            let rec forwardExpand (x, y) =
                let expand = x < n && y < m &&
                             comparison(a.[aStart + x], b.[bStart + y])
                if not expand then (x, y) else forwardExpand (x + 1, y + 1)

            let rec reverseExpand (x, y) =
                let expand = x > 0 && y > 0 &&
                             comparison(a.[aStart + x - 1], b.[bStart + y - 1])
                if not expand then (x, y) else reverseExpand (x - 1, y - 1)

            let rec runForD d =
                let dIsEven = isEven d
                let forwardStartK =
                    if -d < -m then if mIsEven <> dIsEven then -m + 1
                                    else -m
                    else -d

                let forwardEndK =
                    if d > n then if nIsEven <> dIsEven then n - 1
                                    else n
                    else d

                let rec forwardUpdateK k =
                    if k > forwardEndK then None else

                    let kIndex = offset + k
                    let orientation, (x0, y0), (x1, y1) =
                        if k = forwardStartK ||
                            k <> forwardEndK &&
                            vForward.[kIndex - 1] < vForward.[kIndex + 1] then

                            let x0 = vForward.[kIndex + 1]
                            let y0 = x0 - (k + 1)
                            Vertical, (x0, y0), (x0, y0 + 1) |> forwardExpand
                        else
                            let x0 = vForward.[kIndex - 1]
                            let y0 = x0 - (k - 1)
                            Horizontal, (x0, y0), (x0 + 1, y0) |> forwardExpand

                    vForward.[kIndex] <- x1

                    if not deltaIsEven &&
                        k >= delta - (d - 1) &&
                        k <= delta + (d - 1) &&
                        vForward.[kIndex] >= vReverse.[kIndex] then

                        if d = 0 then CompleteMatch else
                            let region = {
                                AStart = x0 + aStart
                                N = (x1 - x0)
                                BStart = y0 + bStart
                                M = (y1 - y0)
                            }
                            (region, Forward, orientation) |> Middle
                        |> Some
                    else forwardUpdateK (k + 2)

                let dPlusDeltaIsEven = d + delta |> isEven
                let reverseStartK =
                    if -d + delta < -m then
                        if mIsEven <> dPlusDeltaIsEven then -m + 1
                        else -m
                    else -d + delta
                let reverseEndK =
                    if d + delta > n then
                        if nIsEven <> dPlusDeltaIsEven then n - 1
                        else n
                    else d + delta

                let rec reverseUpdateK k =
                    if k > reverseEndK then None else

                    let kIndex = offset + k
                    let orientation, (x0, y0), (x1, y1) =
                        if k = reverseEndK ||
                            k <> reverseStartK &&
                            vReverse.[kIndex - 1] < vReverse.[kIndex + 1] then

                            let x1 = vReverse.[kIndex - 1]
                            let y1 = x1 - (k - 1)
                            Vertical, (x1, y1 - 1) |> reverseExpand, (x1, y1)
                        else
                            let x1 = vReverse.[kIndex + 1]
                            let y1 = x1 - (k + 1)
                            Horizontal, (x1 - 1, y1) |> reverseExpand, (x1, y1)

                    vReverse.[kIndex] <- x0

                    if deltaIsEven &&
                        k >= -d && k <= d &&
                        vForward.[kIndex] >= vReverse.[kIndex] then

                        if d = 0 then CompleteMatch else
                            let region = {
                                AStart = x0 + aStart
                                N = (x1 - x0)
                                BStart = y0 + bStart
                                M = (y1 - y0)
                            }
                            (region, Reverse, orientation) |> Middle
                        |> Some
                    else reverseUpdateK (k + 2)

                match forwardUpdateK forwardStartK with
                | Some x -> Some x
                | None -> match reverseUpdateK reverseStartK with
                            | Some x -> Some x
                            | None -> runForD (d + 1)

            runForD 0

        let rec getSnakes region = async {
            let { AStart = aStart; N = n; BStart = bStart; M = m } = region
            match n, m with
            | 0, 0 -> return Seq.empty
            | 0, _ ->
                return seq {
                    for k in 0 .. m - 1 do
                        let middle = { AStart = aStart; N = 0;
                                        BStart = bStart + k; M = 1}
                        yield middle, Forward, Vertical
                }
            | _, 0 ->
                return seq {
                    for k in 0 .. n - 1 do
                        let middle = { AStart = aStart + k; N = 1;
                                        BStart = bStart; M = 0 }
                        yield middle, Forward, Horizontal
                }
            | _, _ ->
                match getMiddleSnake region |> Option.get with
                | CompleteMatch ->  return Seq.empty
                | Middle (middle, direction, orientation) ->
                    let top =
                        { region with N = middle.AStart - aStart
                                      M = middle.BStart - bStart }
                    let bottom = {
                        AStart = aStart + (middle.AStart - aStart) + middle.N
                        N = n - (middle.AStart - aStart) - middle.N
                        BStart = bStart + (middle.BStart - bStart) + middle.M
                        M = m - (middle.BStart - bStart) - middle.M
                    }

                    let! getTopSnakes = getSnakes top |> Async.StartChild
                    let! getBottomSnakes = getSnakes bottom |> Async.StartChild
                    let! topSnakes = getTopSnakes
                    let! bottomSnakes = getBottomSnakes

                    return seq {
                        yield! topSnakes
                        yield (middle, direction, orientation)
                        yield! bottomSnakes
                    }
        }

        let toDiff = function
            | region, Forward, Horizontal -> Deletion region.AStart
            | region, Forward, Vertical ->
                Insertion(region.AStart, b.[region.BStart])
            | region, Reverse, Horizontal ->
                Deletion <| region.AStart + region.N - 1
            | region, Reverse, Vertical ->
                let i = region.AStart + region.N
                Insertion(i, b.[region.BStart + region.M - 1])

        let offset o = function
            | Deletion index -> Deletion(index + o), o - 1
            | Insertion (index, element) -> Insertion(index + o, element), o + 1

        let! snakes =
            getSnakes { AStart = 0; N = a.Count; BStart = 0; M = b.Count }

        return snakes
               |> Seq.map toDiff
               |> Seq.mapFold offset 0
               |> fst
               |> List.ofSeq
               |> List.toSeq
    }

type MyersWithoutEquality<'a, 'b>(comparison : 'a * 'b -> bool) =
    member __.GetDifferences(a, b) = Myers.getDifferences comparison a b

type Myers< 'a when 'a : equality>(comparison) =
    inherit MyersWithoutEquality<'a, 'a>(comparison)

    static let d = (fun (x, y) -> x.Equals(y)) |> Myers<_>

    static member ApplyDifferences(insert, delete, differences, state) =
        (fun c -> function
            | Insertion (i, element) -> insert i element c
            | Deletion i -> delete i c)
        |> Seq.fold <|| (state, differences)
    static member Default = d
