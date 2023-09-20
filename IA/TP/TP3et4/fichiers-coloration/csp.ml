
let rec voisins p1 p2 = function
	 [] -> false;
	| (x1,x2) :: g' -> if((p1=x1 && p2=x2) || (p1=x2 && p2=x1)) then true
																else voisins p1 p2 g'
;;
let rec dansLstSom p = function
	| [] -> false
	| x::l'-> if(p=x) then true
					else dansLstSom p l'
;;
let rec lstSom g = match g with
	 [] -> failwith "list vide"
	| (x1,x2) :: [] -> x1 :: x2 :: []
	| (x1,x2) :: g' -> let lr = (lstSom g') in (match ((dansLstSom x1 lr),(dansLstSom x2 lr)) with
		|(false,false) -> x1::x2::lr
		|(true,false) -> x2::lr
		|(false,true) -> x1::lr
		|(true,true) -> lr
		)
;;

let rec listeVoisin g p = match g with
	 [] -> []
    | (x1,x2) :: g' -> let lr = (listeVoisin g' p) in if(p=x1) then if(dansLstSom x2 lr) then lr
                                                                                            else x2::lr
													else if(p=x2) then if (dansLstSom x1 lr) then lr
																								else x1::lr
															else lr
;;


let rec triAlpha a l = match l with
	[] -> [a]
	|x::l'-> if(a<x) then a::l
					else x:: (triAlpha a l')
;;

let rec triAlphaLst  = function
	[] -> []
	| x :: l' -> let lr = (triAlphaLst l') in triAlpha x lr
;;

(*à partir d'ici je ne suis plus l'auteur du code*)
let rec enumC p g lC li =
	let rec aux lC i= (match lC with
		 [] -> true
		| (p2,i2) :: lC' ->if ((voisins p p2 g) && i=i2) then false else (aux lC' i))
	in
	match li with
	 [] -> Printf.printf "\n";()
	| i :: li' -> Printf.printf "%s%d" p i;if (aux lC i) then (Printf.printf "ok ";enumC p g lC li') else (Printf.printf"no ";enumC p g lC li')
;;

let rec nbC p g lC li =
	let rec aux lC i= (match lC with
		 [] -> true
		| (p2,i2) :: lC' ->if ((voisins p p2 g) && i=i2) then false else (aux lC' i))
	in
	match li with
	 [] -> Printf.printf "\n";0
	| i :: li' -> if (aux lC i) then (1+nbC p g lC li') else (nbC p g lC li')
;;

let backtrack g cMax =
	let rec makeLi i = match i with
		 0 -> []
		| _ -> (makeLi (i-1)) @ [i]
	in
	let rec parcourtlV lV s = match lV with
		 [] -> false
		| s2::lV' -> if s=s2 then true else parcourtlV lV' s
	in
	let rec colPossible (s,c) lC lV = match lC with (* D,2 (A,1	B,2 C,1) (B,C,E)*)
		 [] -> true
		| (s2,c2) ::lC' -> if (parcourtlV lV s2) then (if c=c2 then false else true && colPossible (s,c) lC' lV) else colPossible (s,c) lC' lV
	in
	let rec aux lS lC li n err= match lS with
		 [] -> (lC,n,err)
		| s::lS' -> match li with
			 [] -> ([],n,err+1)
			| c :: li' ->  if (colPossible (s,c) lC (listeVoisin g s)) then (let (ret,retn,rerr) = aux lS' ((s,c)::lC) (makeLi cMax) (n+1) err in if ret =[] then (aux lS lC li' (retn) rerr) else (ret,retn,rerr)) else (aux lS lC li' (n+1) err)
	in aux (triAlphaLst(lstSom g)) [] (makeLi cMax) 0 0
;;
