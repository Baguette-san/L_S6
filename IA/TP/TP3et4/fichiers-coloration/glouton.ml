



(* TP 3 *)

let rec couleursV lSommetsC sTest s = match lSommetsC with
  | [] -> 1
  | (x,c) :: ls -> if x=s then couleursV ls sTest s else
                    if x=sTest then c else couleursV ls sTest s
;;

let rec voisinsEtCouleur lSommetsC lVoisins s = match lVoisins with
   | [] -> [1]
   | x::ls -> (couleursV lSommetsC x s) :: voisinsEtCouleur lSommetsC ls s
;;

let listeCouleursVoisins graphe lSommetsC s =
  let lVoisins = sommetsVoisins graphe s in
  let l = voisinsEtCouleur lSommetsC lVoisins s in
  List.sort_uniq compare(l)
;;

let rec appartient e l = match l with
  | [] -> false
  | x::ls -> if x=e then true else appartient e ls
;;

let rec plusPetitEntier listeCouleur entier = let liste=List.sort_uniq compare(listeCouleur) in match liste with
  | [] -> entier
  | l -> if appartient entier listeCouleur then (plusPetitEntier l (entier+1)) else entier
;;

let rec calculcmax lSommetsC max = match lSommetsC with
  | [] -> max
  | (x,c)::ls -> if c > max then calculcmax ls c else calculcmax ls max
;;

let rec correction lSommetsC = match lSommetsC with
  | []->[]
  | (x,c)::ls-> (x,c-1)::correction ls;;


let gloutonSansH graphe = let lSommets = listSommetsGraphe graphe in
  let rec corpDeFct lSommets lSommetsC = match lSommets with
    | [] -> []
    | x::ls -> let couple = (x, plusPetitEntier(listeCouleursVoisins graphe lSommetsC x) 1 ) in couple :: corpDeFct ls ([couple]@lSommetsC)
  in
  let lSommetsCFinal = List.rev(correction(corpDeFct lSommets []))
  in
  let cmax = calculcmax lSommetsCFinal 0
  in
  (lSommetsCFinal, cmax, lSommets)
;;

let rec nSommet graphe  = function
    | [] -> []
    | x::ls -> (x, List.length(sommetsVoisins graphe x))::nSommet graphe ls
;;

let rec ins e l= match l with
  | [] -> [e]
  | (x,n)::ls -> let a,b = e in if b>n then e::l else (x,n) :: ins e ls
;;

let tri graphe =
  let rec tri2 lSommets lSommetsTrie = match lSommets with
    | [] -> lSommetsTrie
    | x::ls -> tri2 ls (ins x lSommetsTrie)
    in
  let lSommets = listSommetsGraphe graphe in
  let lSommetDeg = nSommet graphe lSommets in
  let lSommetsTrie = [] in
  tri2 lSommetDeg lSommetsTrie
;;


let gloutonDeg graphe = let lSommets,_ = List.split(tri(graphe)) in
  let rec corpDeFct lSommets lSommetsC = match lSommets with
    | [] -> []
    | x::ls -> let couple = (x, plusPetitEntier(listeCouleursVoisins graphe lSommetsC x) 1 ) in couple :: corpDeFct ls ([couple]@lSommetsC)
  in
  let lSommetsCFinal = List.rev(correction(corpDeFct lSommets []))
  in
  let cmax = calculcmax lSommetsCFinal 0
  in
  (lSommetsCFinal, cmax, lSommets)
;;




(*
let gloutonDSat sommets =
*)
