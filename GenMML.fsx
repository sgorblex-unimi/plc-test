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
let gen : Gen<tyexpr> =
    let rec genT (ty: typ) (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
        let genCst (ty: typ) : Gen<tyexpr> =
            match ty with
            | TypI -> gen { return! Gen.map CstI (Gen.choose (-100,100)) }
            | TypB -> gen { return! Gen.map CstB Arb.generate<bool> }
            | _ -> failwith "cannot generate a constant of this type"
        // to be removed
        let rec genZero (ty: typ) env =
            match Map.tryFind ty env with
            | None -> genCst ty
            | Some l -> if List.length l = 0 then failwith "invalid genenv" else Gen.frequency [(1, genCst ty); (3, Gen.map Var (Gen.elements l))]
        let genPrim (ty: typ) (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            let genAnd (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
                gen { let! s1 = Gen.choose (0, s)
                      let s2 = s - s1
                      let! e1 = genT TypB env n m s1
                      let! e2 = genT TypB env n m s2
                      return Prim ("&", e1, e2) }
            match ty with
            | TypI -> failwith "not implemented yet"
            // | TypB -> Gen.oneof (List.map (fun g -> g env n m s) [genAnd; genLess; genEquals])
            | TypB -> Gen.oneof (List.map (fun g -> g env n m s) [genAnd])
            | _ -> failwith "cannot generate a primitive for this type"
        let genLetVar (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            // gen { let! varType = Gen.elements [TypI; TypB]
            gen { let! varType = Gen.elements [TypB]
                  let! s1 = Gen.choose (0, s)
                  let s2 = s - s1
                  let nn = n+1
                  let varName = "v" + string nn
                  let newEnv = addenv varType varName env
                  let! e1 = (if varType = TypB then genT TypB else genT TypI) env nn m s1
                  let! e2 = genT TypB newEnv nn m s2
                  return Let (varName, e1, e2) }
        let genIfElse (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            gen { let! t1 = Gen.choose (0, s)
                  let! t2 = Gen.choose (0, s)
                  let (s1, s2, s3) = if t1<t2 then (t1, t2-t1, s-t2) else (t2, t1-t2, s-t1)
                  let! e1 = genT TypB env n m s1
                  let! e2 = genT TypB env n m s2
                  let! e3 = genT TypB env n m s3
                  return If (e1, e2, e3) }
        let genFun (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            gen { let! s1 = Gen.choose (0, s)
                  let s2 = s - s1
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
                  let! e1 = (if funTypeIn = TypB then genT TypB else genT TypI) newEnvVar nn m s1
                  let! e2 = (if funTypeOut = TypB then genT TypB else genT TypI) newEnvFun n nm s2
                  return Letfun (funName, varName, funTypeIn, e1, funTypeOut, e2) }
        // note: no function recursion allowed
        let genCall (funs: Map<typ, string list>) (env: genenv) (n: int) (m: int) (s: int) : Gen<tyexpr> =
            gen { let! funType = Gen.frequency [(List.length (Map.find TypB funs), Gen.constant TypB ); (List.length (Map.find TypI funs), Gen.constant TypI )]
                  let! funName = Gen.elements (Map.find funType funs)
                  let! e = genT funType env n m s
                  return Call (Var funName, e) }
        match s with
        | 0 -> genZero ty env
        | z when z>0 ->
               let gens_0 = match ty with
                                | TypB -> [(2, genPrim TypB); (1, genLetVar); (1, genIfElse); (1, genFun)]
                                | TypI -> []
                                | _ -> failwith "cannot generate such type"
               let (funs, thereAreFuns) = funsToType ty env
               let gens = if thereAreFuns then (2, genCall funs) :: gens_0 else gens_0
               Gen.frequency (gens |> List.map (fun (w, ge) -> (w, ge env n m (s-1))))
        | _ -> failwith "invalid size"
    // gen { let! genType = Gen.elements [TypB; TypI]
    gen { let! genType = Gen.elements [TypB]
          return! Gen.sized (genT genType emptyGenenv 0 0) }

let go n = gen |> Gen.sample n 1
let gogo n = let a = go n in List.zip a (List.map (fun l -> eval l []) a)
