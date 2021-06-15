module GenMML

#r "FsCheck.dll"
open FsCheck
open TypedFun




// Generator Environment
type genenv = Map<typ, string list>
let emptyGenenv = Map.empty

// Adds the given name of the given type to the given environment
let addenv (addtyp: typ) (name: string) (env: genenv) : genenv =
    Map.add addtyp (name :: (defaultArg (Map.tryFind addtyp env) [])) env


// Returns a map containing functions with the desired return type, with argument type as key and list of names as value.
// The second return value represents the existence of such functions.
let funsToType (lookTyp: typ) (env: genenv) =
    let funsFromType (ty: typ) : string list =
        defaultArg (Map.tryFind (TypF (ty, lookTyp)) env ) []
    let b = funsFromType TypB
    let i = funsFromType TypI
    if List.length b <> 0 || List.length i <> 0 then ((Map.empty |> Map.add TypB b |> Map.add TypI i), true) else (Map.empty, false)


// TODO: ns before specific gen call
// TODO: let cleanup (avoid unnecessary lets)
let gen =
    let rec genT (ty: typ) (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let genCst (ty: typ) : Gen<tyexpr> =
            match ty with
            | TypI -> gen { return! Gen.map CstI (Gen.choose (-100,100)) }
            | TypB -> gen { return! Gen.map CstB Arb.generate<bool> }
            | _ -> failwith "cannot generate a constant of this type"
        // to be removed
        let rec genI (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> = genCst TypI
        let rec genZeroB env =
            match Map.tryFind TypB env with
            | None -> genCst TypB
            | Some l -> if List.length l = 0 then failwith "invalid genenv" else Gen.frequency [(1, genCst TypB); (3, Gen.map Var (Gen.elements l))]
        // TODO: useful?
        let genPrimB (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            let genAnd (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
                let ns = s-1
                gen { let! s1 = Gen.choose (0, ns)
                      let s2 = ns - s1
                      let! e1 = genT TypB env n m s1
                      let! e2 = genT TypB env n m s2
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
                  let! e1 = (if varType = TypB then genT TypB else genI) env nn m s1
                  let! e2 = genT TypB newEnv nn m s2
                  return Let (varName, e1, e2) }
        let genIfElse (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            let ns = s-1
            gen { let! t1 = Gen.choose (0, ns)
                  let! t2 = Gen.choose (0, ns)
                  let (s1, s2, s3) = if t1<t2 then (t1, t2-t1, ns-t2) else (t2, t1-t2, ns-t1)
                  let! e1 = genT TypB env n m s1
                  let! e2 = genT TypB env n m s2
                  let! e3 = genT TypB env n m s3
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
                  let! e1 = (if funTypeIn = TypB then genT TypB else genI) newEnvVar nn m s1
                  let! e2 = (if funTypeOut = TypB then genT TypB else genI) newEnvFun n nm s2
                  return Letfun (funName, varName, funTypeIn, e1, funTypeOut, e2) }
        // note: no function recursion allowed
        let genCall (funs: Map<typ, string list>) (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            let ns = s-1
            gen { let! funType = Gen.frequency [(List.length (Map.find TypB funs), Gen.constant TypB ); (List.length (Map.find TypI funs), Gen.constant TypI )]
                  let! funName = Gen.elements (Map.find funType funs)
                  let! e = genT funType env n m ns
                  return Call (Var funName, e) }
        match s with
        | 0 -> genZeroB env
        | z when z>0 ->
               let (funs, thereAreFuns) = funsToType TypB env
               let gens = [(1, genPrimB env n m s); (1, genLetVar env n m s); (1, genIfElse env n m s); (1, genFun env n m s)]
               Gen.frequency (if thereAreFuns then (2, genCall funs env n m s) :: gens else gens)
        | _ -> failwith "invalid size"
    Gen.sized (genT TypB emptyGenenv 0 0)

let go n = gen |> Gen.sample n 1
let gogo n = let a = go n in List.zip a (List.map (fun l -> eval l []) a)
