namespace Program

open System

module Maybe =
  type Maybe internal () =
    member this.Bind(m: option<'T>, f: 'T -> option<'U>): option<'U> = Option.bind f m
    member this.Return(v: 'T): option<'T> = Some v

  let maybe = new Maybe()

module ListM =
  let rec private bind m f =
    match m with
      [] -> []
    | x :: xs -> List.append (f x) (bind xs f)

  type ListM internal () =
    member this.Bind(m: list<'T>, f: 'T -> list<'U>): list<'U> = bind m f
    member this.Return(v: 'T): list<'T> = [v]

  let list = new ListM ()

module Main =
  open Maybe
  open ListM

  [<EntryPoint>]
  let main _ =
    let m_test1 =
      maybe {
        let! x = Some 42
        let! y = Some 99
        return x + y
      }
    printfn "%A" m_test1
    assert (m_test1 = Some (42 + 99))

    let m_test2 =
      maybe {
        let! x = Some 42
        let! y = None
        return x + y
      }
    printfn "%A" m_test2
    assert (m_test2 = None)

    let l_test1 =
      list {
        let! i = [1; 2; 3]
        let! s = ["a"; "b"; "c"]
        return (i, s)
      }
    printfn "%A" l_test1
    assert (l_test1 = [1, "a"; 1, "b"; 1, "c"; 2, "a"; 2, "b"; 2, "c"; 3, "a"; 3, "b"; 3, "c"])

    let l_test2 =
      list {
          let! i = [1; 2; 3]
          let! s = []
          return (i, s)
      }
    printfn "%A" l_test2
    assert (l_test2 = [])

    0