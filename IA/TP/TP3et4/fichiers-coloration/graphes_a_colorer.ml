(*****************************************************************************)
(* Des graphes : *************************************************************)
(*****************************************************************************)

(* Un petit graphe en extension : *)

let g1 = ["WA","NA" ; "WA","SA" ; "NA","SA" ; "NA","Q" ; "SA","Q" ; "SA","V" ; "SA","NSW" ; "Q","NSW" ; "NSW","V" ]


(* Un autre petit graphe pour vérifier le retour arrière *)

let g2 = ["A","B" ; "A","E" ; "B","C" ; "B","D" ; "B","E" ; "C","D" ; "D","E"]

let g7=[("h","a");("h","d");("h","g");("b","c");("b","e");("b","f");("b","a");("c","f");("d","e");("d","a");("d","g");("e","g");("f","i")]

(* Construction à partir d un fichier au format "DIMACS standard" - décommenter les 4 dernières lignes *)

let rec split s = try let i = String.index_from s 0 ' '
	in (String.sub s 0 i)::(split (String.sub s (i+1) ((String.length s )-(i+1))))
	with Not_found -> [s]

let lire_graphe (nom_fichier : string) : ('a * 'a) list =
(* retourne la liste des arêtes *)
	let ref_l_aretes = ref [] and flux = open_in nom_fichier
	in try
		while true; do match (split (input_line flux)) with
			  "e"::x::y::[] -> ref_l_aretes := (int_of_string x, int_of_string y)::!ref_l_aretes
			| _ -> ()
  		done ; !ref_l_aretes
		with End_of_file -> close_in flux ; !ref_l_aretes


(*
let myciel3 = lire_graphe "myciel3.col" (* 11 sommets *)
let myciel4 = lire_graphe "myciel4.col" (* 23 sommets , 5 couleurs *)
let myciel5 = lire_graphe "myciel5.col" (* 47 sommets *)
let miles750 = lire_graphe "miles750.col" (* 128 sommets *)
*)
;;

let rec sommetsAdjacent lSommets s1 s2 = match lSommets with
  | [] -> false
  | (x,y) :: ls -> if (x,y) = (s1,s2) || (x,y) = (s2,s1)
                      then true
                      else sommetsAdjacent ls s1 s2
;;

let listSommetsGraphe lSommets = let l1,l2 = List.split lSommets in List.sort_uniq compare (l1@l2);;

let rec sommetsVoisins lSommets s = let l = listSommetsGraphe lSommets in
	let l2 = List.sort_uniq compare(l) in
	let rec filtreS l2 = match l2 with
		| [] -> []
		| x::ls -> if x=s then filtreS ls else
												if sommetsAdjacent lSommets s x
												then x :: filtreS ls
												else filtreS ls
	in filtreS l2
;;
