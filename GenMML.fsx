module GenMML

#r "FsCheck.dll"
open FsCheck
open TypedFun



let genCstI =
    gen { return! Gen.map CstI (Gen.choose (-100,100)) }

let genCstB = Gen.elements [CstB true; CstB false]

// generator environment
type genenv = Map<typ, string list>
let emptyGenenv = Map.empty

// TODO: substitute option pattern matching with default
let addenv (addtyp: typ) (name: string) (env: genenv) : genenv =
    match Map.tryFind addtyp env with
    | Some l -> Map.add addtyp (name :: l) env
    | None -> Map.add addtyp [name] env

// TODO: convert to map
type funOfArgType = { tyB: string list; tyI: string list }
// returns a record of function name lists of the desired type present in env, relatively to their argument type. If no such functions are present, the record field will contain the empty list.
let listTypeFun (lookTyp: typ) (env: genenv) =
    {tyB = (defaultArg (Map.tryFind (TypF (TypB, lookTyp)) env ) []); tyI = (defaultArg (Map.tryFind (TypF (TypI, lookTyp)) env ) [])}

// TODO: generalize function for both types (?)
let rec genZeroB env =
    match Map.tryFind TypB env with
    | None -> genCstB
    | Some l -> if List.length l = 0 then failwith "invalid genenv" else Gen.frequency [(1, genCstB); (3, Gen.map Var (Gen.elements l))]

// TODO: genI
// TODO: make genI and genB one parameterized function
// TODO: ns before specific gen call
// let cleanup (avoid unnecessary lets)
let rec genI (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> = gen { return CstI 42 }

let rec genB (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
    // TODO: useful?
    let genPrimB (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let genAnd (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            let ns = s-1
            gen { let! s1 = Gen.choose (0, ns)
                  let s2 = ns - s1
                  let! e1 = genB env n m s1
                  let! e2 = genB env n m s2
                  return Prim ("&", e1, e2) }
        Gen.oneof [genAnd env n m s]
     // Gen.oneof [genEquals; genLess; genAnd]
    let genLetVar (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let ns = s-1
        // gen { let! varType = Gen.elements [TypI; TypB]
        gen { let! varType = Gen.elements [TypB]
              let! s1 = Gen.choose (0, ns)
              let s2 = ns - s1
              let nn = n+1
              let varName = "v" + string nn
              let newEnv = addenv varType varName env
              let! e1 = (if varType = TypB then genB else genI) env nn m s1
              let! e2 = genB newEnv nn m s2
              return Let (varName, e1, e2) }
    let genIfElse (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let ns = s-1
        gen { let! t1 = Gen.choose (0, ns)
              let! t2 = Gen.choose (0, ns)
              let (s1, s2, s3) = if t1<t2 then (t1, t2-t1, ns-t2) else (t2, t1-t2, ns-t1)
              let! e1 = genB env n m s1
              let! e2 = genB env n m s2
              let! e3 = genB env n m s3
              return If (e1, e2, e3) }
    let genFun (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let ns = s-1
        gen { let! s1 = Gen.choose (0, ns)
              let s2 = ns - s1
              // let! funTypeIn = Gen.elements [TypI; TypB]
              let! funTypeIn = Gen.elements [TypB]
              // let! funTypeOut = Gen.elements [TypI; TypB]
              let! funTypeOut = Gen.elements [TypB]
              let nm = m+1
              let funName = "f" + string nm
              let nn = n+1
              let varName = "v" + string nn
              let newEnvVar = addenv funTypeIn varName env
              let newEnvFun = addenv (TypF (funTypeIn, funTypeOut)) funName env
              let! e1 = (if funTypeIn = TypB then genB else genI) newEnvVar nn m s1
              let! e2 = (if funTypeOut = TypB then genB else genI) newEnvFun n nm s2
              return Letfun (funName, varName, funTypeIn, e1, funTypeOut, e2) }
    // note: no function recursion allowed
    let genCall (funs: funOfArgType) (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let ns = s-1
        gen { let! funType = Gen.frequency [(List.length funs.tyB, gen { return TypB }); (List.length funs.tyI, gen { return TypI })]
              let! funName = Gen.elements (if funType=TypB then funs.tyB else funs.tyI)
              let! e = (if funType=TypB then genB else genI) env n m ns
              return Call (Var funName, e) }
    match s with
    | 0 -> genZeroB env
    | z when z>0 ->
           let funs = listTypeFun TypB env
           let gens = [(1, genPrimB env n m s); (1, genLetVar env n m s); (1, genIfElse env n m s); (1, genFun env n m s)]
           Gen.frequency (if List.length funs.tyB + List.length funs.tyI <> 0 then (2, genCall funs env n m s) :: gens else gens)
    | _ -> failwith "invalid size"

let gen =
    Gen.sized (genB emptyGenenv 0 0)

let go n = gen |> Gen.sample n 1
let gogo n = let a = go n in List.zip a (List.map (fun l -> eval l []) a)
