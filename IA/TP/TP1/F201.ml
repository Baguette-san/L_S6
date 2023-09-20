(* Ce fichier contient des graphes sur lesquels tester un *)
(* algorithme de recherche dans un graphe d etat *)

let initGA = "A" ;;
let opPossGA = ( function
	"B" -> [ 'r',182,"D" ; 'd',104,"E" ]
|	"H" -> [ 'c',26,"F" ]
|	"A" -> [ 'm',26,"C" ; 'd',104,"B" ; 'r',390,"J" ]
|	"C" -> [ 'm',26,"F" ; 'n',78,"D" ; 'r',156,"H" ]
|	"E" -> [ 'r',104,"I" ; 'n',78,"J" ]
|	"F" -> [ 'd',78,"G" ; 'f',26,"H" ]
|	"D" -> [ 'd',156,"I" ]
|	 _ -> [] );;
let hEtatGA = ( function
	"B" -> 78
|	"H" -> 130
|	"G" -> 26
|	"A" -> 182
|	"C" -> 130
|	"E" -> 78
|	"F" -> 52
|	"D" -> 104
|	 _ -> 0 );;
let estButGA = ( function "I" | "J" -> true | _ -> false ) ;;


let initGB = "A" ;;
let opPossGB = ( function
	"B" -> [ 'n',156,"D" ; 'd',520,"F" ]
|	"G" -> [ 'd',52,"H" ]
|	"A" -> [ 'm',182,"B" ; 'r',104,"C" ]
|	"C" -> [ 'n',130,"E" ; 'r',78,"D" ]
|	"E" -> [ 'n',312,"G" ; 'r',286,"F" ; 'm',364,"H" ]
|	"F" -> [ 'm',260,"G" ; 'r',208,"H" ]
|	"D" -> [ 'n',390,"F" ]
|	 _ -> [] );;
let hEtatGB = ( function
	"B" -> 468
|	"A" -> 494
|	"G" -> 78
|	"E" -> 260
|	"C" -> 312
|	"D" -> 208
|	 _ -> 0 );;
let estButGB = ( function "F"|"G" | "H" -> true | _ -> false ) ;;


let initGC = "A" ;;
let opPossGC = ( function
	"B" -> [ 'r',182,"D" ; 'd',104,"E" ]
|	"H" -> []
|	"A" -> [ 'm',26,"C" ; 'd',104,"B" ; 'r',390,"J" ]
|	"C" -> [ 'm',26,"F" ; 'n',78,"D" ; 'r',156,"H" ]
|	"E" -> [ 'r',104,"I" ; 'n',78,"J" ]
|	"F" -> [ 'd',78,"G" ; 'f',26,"H" ]
|	"D" -> [ 'd',156,"I" ]
|	 _ -> [] );;
let hEtatGC = ( function
	"B" -> 78
|	"H" -> 78
|	"G" -> 52
|	"A" -> 182
|	"C" -> 130
|	"E" -> 78
|	"F" -> 52
|	"D" -> 104
|	 _ -> 0 );;
let estButGC = ( function  _ -> false ) ;;

