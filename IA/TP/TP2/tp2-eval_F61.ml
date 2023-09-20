


let min l = if l=[] then failwith "liste vide" else
    let rec mini x l' = match l' with 
        | [] -> x
        | e::l'' -> if e < x then mini e l'' else mini x l''
    in mini (List.hd l) (List.tl l)  
;;

let max l = if l=[] then failwith "liste vide" else
    let rec maxi x l' = match l' with 
        | [] -> x
        | e::l'' -> if e > x then maxi e l'' else maxi x l''
    in maxi (List.hd l) (List.tl l)  
;;

let maxListTuple l = if l=[] then failwith "liste vide" else
    let rec maxiTuple (xC,xN) l' = match l' with 
        | [] -> (xC,xN)
        | (eC,eN)::l'' -> if eC > xC then maxiTuple (eC,eN) l'' else maxiTuple (xC,xN) l''
    in maxiTuple (List.hd l) (List.tl l)  
;;


let rec minimax e (*etat*) p (*profondeur*) ef (*etat fils*) eval (*evaluation*) = let lf (*liste fils*)  = ef e in match lf with
    | [] -> eval e 
    | _ -> let lv (*liste minimax fils*) = List.map (fun x -> minimax (snd x) (p+1) ef eval) lf    
    in if (p mod 2) = 0 then max lv else min lv
;;


let rec recherche pmax (*prof max*) e p ef eval  = 
    let lf  = if pmax > p then ef e else [] in match lf with
    | [] -> eval e
    | _ -> let lv (*liste recherche fils*) = List.map(fun x -> recherche pmax (snd x) (p+1) ef eval) lf    
    in if p mod 2 = 0 then max lv else min lv
;;

let rec meilleurCoup pmax e p ef eval = 
    let lf  = if pmax > p then ef e else [] in 
    let rec listMeilleurs pmax p ef eval lf = match lf with
    | [] -> []
    | (pCout, x)::lf' -> ( recherche pmax x p ef eval, pCout) :: listMeilleurs pmax p ef eval lf' in    
    maxListTuple (listMeilleurs pmax (p+1) ef eval lf)
;;
(* Ce fichier définit en OCAML des arbres sur lesquels tester une évaluation type minimax *)

let etatInitialA1 = "0" ;;
let etatsFilsA1 = ( function
	"0" -> [ 'f',"Z" ; 'g',"X" ; 'h',"T" ; 'i',"C" ; 'u',"Q" ]
|	"C" -> [ 'i',"U" ; 'w',"L" ]
|	"F" -> [ 'w',"E" ; 'x',"G" ]
|	"T" -> [ 'f',"H" ; 'w',"D" ]
|	"W" -> [ 'i',"V" ; 'x',"N" ]
|	"X" -> [ 'g',"Y" ; 'i',"M" ; 'w',"W" ]
|	"Z" -> [ 'm',"I" ; 'n',"O" ; 'r',"F" ; 'v',"K" ]
|	 _ -> [] );;
let evaluationA1 = ( function
	"0" -> -44
|	"C" -> -4
|	"D" -> 12
|	"E" -> 22
|	"F" -> -24
|	"G" -> -18
|	"H" -> 4
|	"I" -> 20
|	"K" -> 28
|	"L" -> 16
|	"M" -> 8
|	"N" -> -28
|	"O" -> 12
|	"Q" -> -8
|	"T" -> 16
|	"U" -> 4
|	"V" -> 16
|	"W" -> 16
|	"X" -> -6
|	"Y" -> 10
|	"Z" -> -14
|	 _ -> 0 );;


let etatInitialA2 = "0" ;;
let etatsFilsA2 = ( function
	"0" -> [ 'b',"O" ; 'o',"C" ; 't',"X" ; 'v',"Z" ; 'w',"V" ]
|	"D" -> [ 't',"T" ; 'z',"U" ]
|	"E" -> [ 's',"Y" ; 'z',"J" ]
|	"O" -> [ 'b',"F" ; 's',"D" ; 't',"S" ]
|	"V" -> [ 'e',"E" ; 'f',"Q" ; 'k',"I" ; 'r',"W" ]
|	"X" -> [ 's',"M" ; 't',"N" ]
|	"Z" -> [ 's',"L" ; 'w',"B" ]
|	 _ -> [] );;
let evaluationA2 = ( function
	"0" -> -73
|	"B" -> 23
|	"C" -> -1
|	"D" -> 47
|	"E" -> -33
|	"F" -> 35
|	"I" -> -25
|	"J" -> -21
|	"L" -> 39
|	"M" -> 47
|	"N" -> 23
|	"O" -> 3
|	"Q" -> 71
|	"S" -> 31
|	"T" -> 47
|	"U" -> -41
|	"V" -> -13
|	"W" -> 55
|	"X" -> 7
|	"Y" -> 59
|	"Z" -> 47
|	 _ -> 0 );;


let etatInitialA3 = "0" ;;
let etatsFilsA3 = ( function
	"0" -> [ 'e',"O" ; 'g',"V" ; 'j',"N" ; 'q',"Q" ; 'x',"Z" ]
|	"I" -> [ 'n',"T" ; 's',"F" ]
|	"M" -> [ 'q',"R" ; 's',"P" ]
|	"O" -> [ 'i',"I" ; 'l',"C" ; 'm',"H" ; 'p',"J" ]
|	"Q" -> [ 'n',"E" ; 'q',"G" ]
|	"V" -> [ 'g',"L" ; 'n',"M" ; 'q',"K" ]
|	"Z" -> [ 'e',"A" ; 'n',"Y" ]
|	 _ -> [] );;
let evaluationA3 = ( function
	"0" -> -56
|	"A" -> 13
|	"C" -> 52
|	"E" -> 34
|	"F" -> -17
|	"G" -> 16
|	"H" -> 40
|	"I" -> -26
|	"J" -> -20
|	"K" -> -14
|	"L" -> 25
|	"M" -> 34
|	"N" -> -2
|	"O" -> -11
|	"P" -> -32
|	"Q" -> 4
|	"R" -> 34
|	"T" -> 43
|	"V" -> 1
|	"Y" -> 28
|	"Z" -> 43
|	 _ -> 0 );;

minimax etatInitialA1 0  etatsFilsA1 evaluationA1;;
minimax etatInitialA2 0  etatsFilsA2 evaluationA2;;
minimax etatInitialA3 0  etatsFilsA3 evaluationA3;;
recherche 1 etatInitialA1 0  etatsFilsA1 evaluationA1;;
recherche 1 etatInitialA2 0  etatsFilsA2 evaluationA2;;
recherche 1 etatInitialA3 0  etatsFilsA3 evaluationA3;;
recherche 2 etatInitialA1 0  etatsFilsA1 evaluationA1;;
recherche 2 etatInitialA2 0  etatsFilsA2 evaluationA2;;
recherche 2 etatInitialA3 0  etatsFilsA3 evaluationA3;;
meilleurCoup 2 etatInitialA1 0  etatsFilsA1 evaluationA1;;
meilleurCoup 2 etatInitialA2 0  etatsFilsA2 evaluationA2;;
meilleurCoup 2 etatInitialA3 0  etatsFilsA3 evaluationA3;;
meilleurCoup 10 etatInitialA1 0  etatsFilsA1 evaluationA1;;
meilleurCoup 10 etatInitialA2 0  etatsFilsA2 evaluationA2;;
meilleurCoup 10 etatInitialA3 0  etatsFilsA3 evaluationA3;;

