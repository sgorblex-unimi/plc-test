module GenMML

#r "FsCheck.dll"
open FsCheck
open TypedFun



let genCstI =
    gen { let! a = Gen.choose (-100,100)
          return CstI a }

let genCstB = Gen.elements [CstB true; CstB false]

// generator environment
type genenv = Map<typ, string list>
let emptyGenenv = Map.empty

// todo: substitute option pattern matching with default
let addenv (addtyp: typ) (name: string) (env: genenv) : genenv =
    match Map.tryFind addtyp env with
    | Some l -> Map.add addtyp (name :: l) env
    | None -> Map.add addtyp [name] env

// todo: generalize function for both types (?)
let rec genZeroB env =
    match Map.tryFind TypB env with
    | None -> genCstB
    | Some l -> if List.length l = 0 then failwith "invalid genenv" else Gen.frequency [(1, genCstB); (2, Gen.map Var (Gen.elements l))]

let rec genI (env: genenv) (n: int) (s: int) : Gen<tyexpr> = gen { return CstI 42 }

let rec genB (env: genenv) (n: int) (s: int) : Gen<tyexpr> =
    let genPrimB (env: genenv) (n: int) (s: int) : Gen<tyexpr> =
        let genAnd (env: genenv) (n: int) (s: int) : Gen<tyexpr> =
            let ns = s-1
            gen { let! s1 = Gen.choose (0, ns)
                  let s2 = ns - s1
                  let! e1 = genB env n s1
                  let! e2 = genB env n s2
                  return Prim ("&", e1, e2) }
        Gen.oneof [genAnd env n s]
     // Gen.oneof [genEquals; genLess; genAnd]
    let genLetVar (env: genenv) (n: int) (s: int) : Gen<tyexpr> =
        // gen { let! varType = Gen.elements [TypI; TypB]
        gen { let! varType = Gen.elements [TypB]
              let ns = s-1
              let! s1 = Gen.choose (0, ns)
              let s2 = ns - s1
              let nn = n+1
              let varName = "v" + string nn
              let newEnv = addenv varType varName env
              let! e1 = if varType = TypB then genB env nn s1 else genI env nn s1
              let! e2 = genB newEnv nn s2
              return Let (varName, e1, e2) }
    let genIfElse (env: genenv) (n: int) (s: int) : Gen<tyexpr> =
        gen { let ns = s-1
              let! t1 = Gen.choose (0, ns)
              let! t2 = Gen.choose (0, ns)
              let (s1, s2, s3) = if t1<t2 then (t1, t2-t1, ns-t2) else (t2, t1-t2, ns-t1)
              let! e1 = genB env n s1
              let! e2 = genB env n s2
              let! e3 = genB env n s3
              return If (e1, e2, e3) }
    match s with
    | 0 -> genZeroB env
    // | n when n>0 -> Gen.frequency [(3, genPrimB env n s); (1, genLetVar env n s)]
    | m when m>0 -> Gen.frequency [(1, genPrimB env n s); (1, genLetVar env n s); (1, genIfElse env n s)]
    // | m when m>0 -> Gen.frequency [(1, genPrimB env n s); (1, genLetVar env n s)]
    | _ -> failwith "invalid size"

let gen =
    Gen.sized (genB emptyGenenv 0)

let go n = gen |> Gen.sample n 1
let gogo n = let a = go n in List.zip a (List.map (fun l -> eval l []) a)
