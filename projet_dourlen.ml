(* definition de arbre234 *)
type element = int;;
type ab234 = Vide 
						| Noeud1 of ( element * ab234 * ab234 ) 
						| Noeud2 of ( element * element * ab234 * ab234 * ab234 ) 
						| Noeud3 of ( element * element * element * ab234 * ab234 * ab234 * ab234 );;

(* definition de arbre bicolore *)
type couleur = Rouge | Noir;;
type abic = Empty | VideNoir | Noeud of ( element * couleur * abic * abic );;

(* arbre ecrit au tableau, arbre234 sûr *)
let test1 = Noeud2(10, 20, Noeud1(5,Vide,Vide), Noeud2(11,12, Vide, Vide, Vide), Noeud3(25, 26, 27, Vide, Vide, Vide, Vide));;
let test2 = Noeud2(10, 20, Vide, Noeud2(11,12, Vide, Vide, Vide), Noeud3(25, 26, 27, Vide, Vide, Vide, Vide));;

(* outils de comparaison *)
let comp = compare;;
let ($=$) x y = comp x y = 0;;
let ($<$) x y = comp x y < 0;;
let ($>$) x y = comp x y > 0;;

(* most_left_value of Tree *)
(* utilisation: *)
(* most_left (random_ab234 1000000 5000);; *)
let rec most_left = function
	| Vide -> failwith "l'arbre doit contenir une valeur"
	| Noeud1 ( x, Vide, _) -> x
	| Noeud1 ( _, lt, _) -> most_left lt
	| Noeud2 ( x, _, Vide, _, _) -> x
	| Noeud2 ( _, _, lt, _, _) -> most_left lt
	| Noeud3 ( x, _, _, Vide, _, _, _) -> x
	| Noeud3 ( _, _, _, lt, _, _, _) -> most_left lt;;

(* most_right_value of Tree *)
(* utilisation: *)
(* most_right (random_ab234 1000000 5000);; *)
let rec most_right = function
	| Vide -> failwith "l'arbre doit contenir une valeur"
	| Noeud1 (x , _, Vide) -> x
	| Noeud1 ( _, _, lt) -> most_right lt
	| Noeud2 (_ , x, _, _, Vide) -> x
	| Noeud2 ( _, _, _, _, lt) -> most_right lt
	| Noeud3 ( _, _, x, _, _, _, Vide) -> x
	| Noeud3 ( _, _, _, _, _, _, lt) -> most_right lt;;

(* fonction qui verifie qu'un arbre234 est bien de recherche *)
let rec est_recherche234 = function
	| Vide -> true
	| Noeud1 (_, Vide, Vide) -> true
	| Noeud2 (_, _, Vide, Vide, Vide) -> true
	| Noeud3 (_, _, _, Vide, Vide, Vide, Vide) -> true
	| Noeud1 (x, a1, a2) -> (est_recherche234 a1 && est_recherche234 a2 && x > most_right a1 && x < most_left a2)
	| Noeud2 (x, y, a1, a2, a3) -> (est_recherche234 a1 && est_recherche234 a2 && est_recherche234 a3 && x > most_right a1 && ( x < most_left a2 && y > most_right a2 ) && y < most_left a3)
	| Noeud3 (x, z, y, a1, a2, a3, a4) -> (est_recherche234 a1 && est_recherche234 a2 && est_recherche234 a3 && est_recherche234 a4 && x > most_right a1 && ( x < most_left a2 && z > most_right a2 ) && ( z < most_left a3 && y > most_right a3 ) && y < most_left a4);;

(* compte la hauteur max de l'arbre fournie *)
let rec hauteur_noeud = function
	| Vide -> 0
	| Noeud1(_, a1, a2) -> max (hauteur_noeud a1) (hauteur_noeud a2) + 1
	| Noeud2(_, _, a1, a2, a3) -> max (hauteur_noeud a1) (max (hauteur_noeud a2) (hauteur_noeud a3)) + 1
	| Noeud3(_, _, _, a1, a2, a3, a4) -> max (max (hauteur_noeud a1) (hauteur_noeud a2)) (max (hauteur_noeud a3) (hauteur_noeud a4)) + 1;;

(* renvoie la hauteur si l'arbre est de meme hauteur partout 	*)
(* 										sinon une exception											*)
(* a modifier pour renvoyer true ou false 										*)
(*   (hauteur_noeud a1) + 1																		*)
let rec hauteur = function
	| Vide -> true
	| Noeud1(_, a1, a2) -> if (hauteur_noeud a1) == (hauteur_noeud a2)
								then true
								else false
	| Noeud2(_, _, a1, a2, a3) -> if ((hauteur_noeud a1) == (hauteur_noeud a2) && (hauteur_noeud a2) == (hauteur_noeud a3))
																then true
																else false
	| Noeud3(_, _, _, a1, a2, a3, a4) ->if ((hauteur_noeud a1) == (hauteur_noeud a2) && (hauteur_noeud a2) == (hauteur_noeud a3) && (hauteur_noeud a3) == (hauteur_noeud a4))
																then true
																else false;;

(* verifie si l'arbre fourni en parametres est un arbre 234 *)
let est_234 ab = 
	(hauteur ab && est_recherche234 ab);;

(* Q1.3 *)
(* recherche dans un arbre234 *)
let rec est_dans x = function
	| Vide -> false
	| Noeud1(r, _, _) when r $=$ x -> true
	| Noeud2(r, t, _, _, _) when r $=$ x || t $=$ x -> true
	| Noeud3(r, t, y, _, _, _, _) when r $=$ x || t $=$ x || y $=$ x -> true
	| Noeud1(r , ag, _) when x $<$ r -> est_dans x ag
	| Noeud1(r , _, ad) -> est_dans x ad
	| Noeud2(r , _, ag, _, _) when x $<$ r -> est_dans x ag
	| Noeud2(_ , r, _, _, ad) when x $>$ r -> est_dans x ad
	| Noeud2(_ , _, _, am, _) -> est_dans x am
	| Noeud3(r , _, _, ag, _, _, _) when x $<$ r -> est_dans x ag
	| Noeud3(_ , r, _, _, amg, _, _) when x $<$ r -> est_dans x amg
	| Noeud3(_ , _, r, _, _, amd, _) when x $<$ r -> est_dans x amd
	| Noeud3(_ , _, _, _, _, _, ad) -> est_dans x ad;;

(* testé avec test1 *)
(* transforme un arbre234 en arbre binaire *)
let rec ab234_vers_abic = function 
	| Vide -> Empty
	| Noeud1(x, ag, ad) -> Noeud(x, Noir, (ab234_vers_abic ag), (ab234_vers_abic ad))
	| Noeud2(x, y, ag, am, ad) -> Noeud(y, Noir, Noeud(x, Rouge, (ab234_vers_abic ag), ( ab234_vers_abic am)), (ab234_vers_abic ad))
	| Noeud3(x, y, z, ag, amg, amd, ad) -> Noeud(y, Noir, Noeud(x, Rouge, (ab234_vers_abic ag), (ab234_vers_abic amg)), Noeud(z, Rouge, (ab234_vers_abic amd), (ab234_vers_abic ad)));;

(* testé avec (ab234_vers_abic test1) et redonne test1 *)
(* transforme un arbre binaire en arbre234*)
let rec abic_vers_ab234 = function 
	| Noeud(y, Noir, Noeud(x, Rouge, ag, amg), Noeud(z, Rouge, amd, ad)) -> Noeud3(x, y, z, (abic_vers_ab234 ag), (abic_vers_ab234 amg), (abic_vers_ab234 amd), (abic_vers_ab234 ad))
	| Noeud(y, Noir, Noeud(x, Rouge, ag, am), ad) -> Noeud2(x, y, (abic_vers_ab234 ag), (abic_vers_ab234 am), (abic_vers_ab234 ad))
	| Noeud(x, Noir, ag, Noeud(y, Rouge, am, ad)) -> Noeud2(x, y, (abic_vers_ab234 ag), (abic_vers_ab234 am), (abic_vers_ab234 ad))
	| Noeud(x, Noir, ag, ad) -> Noeud1(x, (abic_vers_ab234 ag), (abic_vers_ab234 ad))
	| Empty -> Vide;;

(* fonction equilibrer permettant de casser les Noeud pour l'insertion *)
let equilibrer = function
	| Noeud1(x, Noeud3(e1, e2, e3, ag, amg, amd, ad), a2) -> Noeud2(e2, x, Noeud1(e1, ag, amg), Noeud1(e3, amd, ad), a2)
	| Noeud1(x, a1, Noeud3(e1, e2, e3, ag, amg, amd, ad)) -> Noeud2(x, e2, a1, Noeud1(e1, ag, amg), Noeud1(e3, amd, ad))
	| Noeud2(x, y, Noeud3(e1, e2, e3, ag, amg, amd, ad), a2, a3) -> Noeud3(e2, x, y, Noeud1(e1, ag, amg), Noeud1(e3, amd, ad), a2, a3)
	| Noeud2(x, y, a2, Noeud3(e1, e2, e3, ag, amg, amd, ad), a3) -> Noeud3(x, e2, y, a2, Noeud1(e1, ag, amg), Noeud1(e3, amd, ad), a3)
	| Noeud2(x, y, a2, a3, Noeud3(e1, e2, e3, ag, amg, amd, ad)) -> Noeud3(x, y, e2, a2, a3, Noeud1(e1, ag, amg), Noeud1(e3, amd, ad))
	| Noeud3(x, y, z, a1, a2, a3, a4) -> Noeud1(y, Noeud1(x, a1, a2), Noeud1(z, a3, a4))
	| a -> a;;

(* fonction permettant l'insertion d'un valeur dans ab234 *)
let inserer v a = 
	let rec insertion_aux = function
		(* cas direct *)
  	| Vide -> Noeud1(v, Vide, Vide)
  	| Noeud1(r, _, _) as a when r $=$ v -> a
  	| Noeud2(r, t, _, _, _) as a when r $=$ v || t $=$ v -> a
  	| Noeud3(r, t, y, _, _, _, _) as a when r $=$ v || t $=$ v || y $=$ v -> a
		(* cas ou il faut inserer *)
		(* normalement ici tout les sous arbres sont Vide *)
		| Noeud1(r , Vide, ad) when v $<$ r -> Noeud2(v, r, Vide, Vide, ad)
		| Noeud1(r, ag, Vide) -> Noeud2(r, v, ag, Vide, Vide)
		| Noeud2(r , y, Vide, am, ad) when v $<$ r -> Noeud3(v, r, y, Vide, Vide, am, ad)
		| Noeud2(y , r, ag, Vide, ad) when v $>$ r -> Noeud3(y, r, v, ag, Vide, Vide, ad)
		| Noeud2(y , r, ag, am, Vide) -> Noeud3(y, v, r, ag, am, Vide, Vide)
		(* cas ou il faut avancé tant que ce n'est pas un Noeud externe *)
  	(* fonction equilibrer a chaque noeud passé  *)
  	| Noeud1(r , ag, ad) when v $<$ r -> equilibrer (Noeud1(r, (insertion_aux ag), ad))
  	| Noeud1(r , ag, ad) -> equilibrer (Noeud1(r, ag, (insertion_aux ad)))
  	| Noeud2(r , y, ag, am, ad) when v $<$ r -> equilibrer (Noeud2(r , y, insertion_aux ag, am, ad))
  	| Noeud2(y , r, ag, am, ad) when v $>$ r -> equilibrer (Noeud2(y , r, ag, am, insertion_aux ad))
  	| Noeud2(y , r, ag, am, ad) -> equilibrer (Noeud2(y , r, ag, insertion_aux am, ad))
  	| Noeud3(r, x, y, ag, amg, amd, ad) when v $<$ r -> equilibrer (Noeud3(r , x, y, insertion_aux ag, amg, amd, ad))
  	| Noeud3(x, r, y, ag, amg, amd, ad) when v $<$ r -> equilibrer (Noeud3(x, r, y, ag, insertion_aux amg, amd, ad))
  	| Noeud3(x , y, r,ag, amg, amd, ad) when v $<$ r -> equilibrer (Noeud3(x , y, r,ag, amg, insertion_aux amd, ad))
  	| Noeud3(x , y, r, ag, amg, amd, ad) -> equilibrer (Noeud3(x , y, r, ag, amg, amd, insertion_aux ad))
		in insertion_aux (equilibrer a);;

(* fonction de creation d'arbre234 aleatoire *)
let random_ab234 bound nb =
	let rec aux acc = function
		| 0 -> acc
		| n -> aux (inserer (Random.int bound) acc) (n-1)
	in aux Vide nb;;

(* fonction d'equilibrage de la racine pour la suppression *)
let equilibrer_supp_rac v = function
	(* cas fig8 *)
	| Noeud1(e1, Noeud1(e2, a1, a2), Noeud1(e3, a3, a4)) -> Noeud3(e2, e1, e3, a1, a2, a3, a4)
	(* cas fig 9 *)
	| Noeud1(e1, Noeud1(e2, a1, a2), Noeud2(e3, e4, a3, a4, a5)) when ((v $<$ e1) || (v $=$ e1)) -> 
		Noeud1(e3, Noeud2(e2, e1, a1, a2, a3), Noeud1(e4, a4, a5))
	| Noeud1(e1, Noeud2(e2, e3, a1, a2, a3), Noeud1(e4, a4, a5)) when (v $>$ e1) -> Noeud1(e3, Noeud1(e2, a1, a2), Noeud2(e1, e4, a3, a4, a5))
	(* cas particulier ou e1 = v ainsi on le descend ou on veux *)
	| Noeud1(e1, Noeud3(e2, e3, e4, a1, a2, a3, a4), Noeud1(e5, a5, a6)) -> Noeud1(e4, Noeud2(e2, e3, a1, a2, a3), Noeud2(e1, e5, a4, a5, a6))
	| Noeud1(e1, Noeud1(e2, a1, a2), Noeud3(e3, e4, e5, a3, a4, a5, a6)) -> Noeud1(e3, Noeud2(e2, e1, a1, a2, a3), Noeud2(e4, e5, a4, a5, a6))
	| a -> a;;

(* fonction d'equilibrage pour la suppression *)
let equilibrer_supp v = function 
	(* cas Noeud2 parent *)
	(* cas v <= e1 *)
	| Noeud2(e1, e2 , Noeud1(e3, a1, a2), Noeud2(e4, e5, a3, a4, a5), e6) when ((v $<$ e1) || (v $=$ e1)) -> Noeud2(e4, e2, Noeud2(e3, e1, a1, a2, a3), Noeud1(e5, a4, a5), e6)
	| Noeud2(e1, e2 , Noeud1(e3, a1, a2), Noeud3(e4, e5, e6, a3, a4, a5, a6), e7) when ((v $<$ e1) || (v $=$ e1)) -> Noeud2(e4, e2, Noeud2(e3, e1, a1, a2, a3), Noeud2(e5, e6, a4, a5, a6), e7)
	| Noeud2(e1, e2, Noeud1(e3, a1, a2), Noeud1(e4, a3, a4), Noeud2(e5, e6, a5, a6, a7)) when ((v $<$ e1) || (v $=$ e1)) -> Noeud2(e4, e5, Noeud2(e3, e1, a1, a2, a3), Noeud1(e2, a4, a5), Noeud1(e6, a6, a7))
	| Noeud2(e1, e2, Noeud1(e3, a1, a2), Noeud1(e4, a3, a4), Noeud3(e5, e6, e7, a5, a6, a7, a8)) when ((v $<$ e1) || (v $=$ e1)) -> Noeud2(e4, e5, Noeud2(e3, e1, a1, a2, a3), Noeud1(e2, a4, a5), Noeud2(e6, e7, a6, a7, a8))
	(* cas v <= e2 *)
	| Noeud2(e1, e2 , Noeud2(e3, e4, a1, a2, a3), Noeud1(e5, a4, a5), e6) when ((v $<$ e2) || (v $=$ e2)) -> Noeud2(e4, e2, Noeud1(e3, a1, a2), Noeud2(e1, e5, a3, a4, a5), e6)
	| Noeud2(e1, e2 , Noeud3(e3, e4, e5, a1, a2, a3, a4), Noeud1(e6, a5, a6), e7) when ((v $<$ e2) || (v $=$ e2)) -> Noeud2(e5, e2, Noeud2(e3, e4, a1, a2, a3), Noeud2(e1, e6, a4, a5, a6), e7)
	| Noeud2(e1, e2, e3, Noeud1(e4, a1, a2), Noeud2(e5, e6, a3, a4, a5)) when ((v $<$ e2) || (v $=$ e2)) -> Noeud2(e1, e5, e3, Noeud2(e4, e2, a1, a2, a3), Noeud1(e6, a4, a5))
	| Noeud2(e1, e2, e3, Noeud1(e4, a1, a2), Noeud3(e5, e6, e7, a3, a4, a5, a6)) when ((v $<$ e2) || (v $=$ e2)) -> Noeud2(e1, e5, e3, Noeud2(e4, e2, a1, a2, a3), Noeud2(e6, e7, a4, a5, a6))
	(* cas ou il faut passer par le noeud du milieu *)
	(* cas v > e2 *)
	| Noeud2(e1, e2 , Noeud2(e3, e4, a1, a2, a3), Noeud1(e5, a4, a5), Noeud1(e6, a6, a7)) when ((v $>$ e2)) -> Noeud2(e4, e5, Noeud1(e3, a1, a2), Noeud1(e1, a3, a4), Noeud2(e2, e6, a5, a6, a7))
	| Noeud2(e1, e2 , Noeud3(e3, e4, e5, a1, a2, a3, a4), Noeud1(e6, a5, a6), Noeud1(e7, a7, a8)) when ((v $>$ e2)) -> Noeud2(e5, e6, Noeud2(e3, e4, a1, a2, a3), Noeud1(e1, a4, a5), Noeud2(e2, e7, a6, a7, a8))
	| Noeud2(e1, e2 , e3, Noeud2(e4, e5, a3, a4, a5), Noeud1(e6, a6, a7)) when ((v $>$ e2)) -> Noeud2(e1, e5, e3, Noeud1(e4, a3, a4), Noeud2(e2, e6, a5, a6, a7))
	| Noeud2(e1, e2 , e3, Noeud3(e4, e5, e6, a3, a4, a5, a6), Noeud1(e7, a7, a8)) when ((v $>$ e2)) -> Noeud2(e1, e6, e3, Noeud2(e4, e5, a3, a4, a5), Noeud2(e2, e7, a6, a7, a8))
	(* | cas gerer ailleure ici inutile  *)
	(* | Noeud2(e1, e2 , Noeud2(e3, e4, a1, a2, a3), Noeud2, Noeud1(e6, a4, a5)) when ((v $>$ e2)) -> Noeud2(e4, e2, Noeud1(e3, a1, a2), Noeud2(e1, e5, a3, a4, a5), e6) *)
	(* | Noeud2(e1, e2 , Noeud2(e3, e4, a1, a2, a3), Noeud3, Noeud1(e6, a4, a5)) when ((v $>$ e2)) -> Noeud2(e4, e2, Noeud1(e3, a1, a2), Noeud2(e1, e5, a3, a4, a5), e6) *)
	(* cas Noeud3 parent *)
	 (* cas (v <= e1) *)
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud2(e5, e6, a3, a4, a5), e7, e8) when ((v $<$ e1) || (v $=$ e1)) -> Noeud3(e5, e2, e3, Noeud2(e4, e1, a1, a2, a3), Noeud1(e6, a4, a5), e7, e8)
  | Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud3(e5, e6, e7, a3, a4, a5, a6), e8, e9) when ((v $<$ e1) || (v $=$ e1)) -> Noeud3(e5, e2, e3, Noeud2(e4, e1, a1, a2, a3), Noeud2(e6, e7, a4, a5, a6), e8, e9)
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud2(e6, e7, a5, a6, a7), e8) when ((v $<$ e1) || (v $=$ e1)) -> Noeud3(e5, e6, e3, Noeud2(e4, e1, a1, a2, a3), Noeud1(e2, a4, a5), Noeud1(e7, a6, a7), e8)
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud3(e6, e7, e8, a5, a6, a7, a8), e9) when ((v $<$ e1) || (v $=$ e1)) -> Noeud3(e5, e6, e3, Noeud2(e4, e1, a1, a2, a3), Noeud1(e2, a4, a5), Noeud2(e7, e8, a6, a7, a8), e9)
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud1(e6, a5, a6), Noeud2(e7, e8, a7, a8, a9)) when ((v $<$ e1) || (v $=$ e1)) -> Noeud3(e5, e6, e7, Noeud2(e4, e1, a1, a2, a3), Noeud1(e2, a4, a5), Noeud1(e3, a6, a7), Noeud1(e8, a8, a9))
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud1(e6, a5, a6), Noeud3(e7, e8, e9, a7, a8, a9, a10)) when ((v $<$ e1) || (v $=$ e1)) -> Noeud3(e5, e6, e7, Noeud2(e4, e1, a1, a2, a3), Noeud1(e2, a4, a5), Noeud1(e3, a6, a7), Noeud2(e8, e9, a8, a9, a10))
	(* cas ou (v $<=$ e2) *)
	| Noeud3(e1, e2, e3, Noeud2(e4, e5, a1, a2, a3), Noeud1(e6, a4, a5), e7, e8) when ((v $<$ e2) || (v $=$ e2)) -> Noeud3(e5, e2, e3, Noeud1(e4, a1, a2), Noeud2(e1, e6, a3, a4, a5), e7, e8)
	| Noeud3(e1, e2, e3, Noeud3(e4, e5, e6, a1, a2, a3, a4), Noeud1(e7, a5, a6), e8, e9) when ((v $<$ e2) || (v $=$ e2))-> Noeud3(e6, e2, e3, Noeud2(e4, e5, a1, a2, a3), Noeud2(e1, e7, a4, a5, a6), e8, e9)
	| Noeud3(e1, e2, e3, e4, Noeud1(e5, a1, a2), Noeud2(e6, e7, a3, a4, a5), e8) when ((v $<$ e2) || (v $=$ e2)) -> Noeud3(e1, e6, e3, e4, Noeud2(e5, e2, a1, a2, a3), Noeud1(e7, a4, a5), e8)
	| Noeud3(e1, e2, e3, e4, Noeud1(e5, a1, a2), Noeud3(e6, e7, e8, a3, a4, a5, a6), e9) when ((v $<$ e2) || (v $=$ e2)) -> Noeud3(e1, e6, e3, e4, Noeud2(e5, e2, a1, a2, a3), Noeud2(e7, e8, a4, a5, a6), e9)
	| Noeud3(e1, e2, e3, e4, Noeud1(e5, a1, a2), Noeud1(e6, a3, a4), Noeud2(e7, e8, a5, a6, a7)) when ((v $<$ e2) || (v $=$ e2)) -> Noeud3(e1, e6, e7, e4, Noeud2(e5, e2, a1, a2, a3), Noeud1(e3, a4, a5), Noeud1(e8, a6, a7))
	| Noeud3(e1, e2, e3, e4, Noeud1(e5, a1, a2), Noeud1(e6, a3, a4), Noeud3(e7, e8, e9, a5, a6, a7, a8)) when ((v $<$ e2) || (v $=$ e2)) -> Noeud3(e1, e6, e7, e4, Noeud2(e5, e2, a1, a2, a3), Noeud1(e3, a4, a5), Noeud2(e8, e9, a6, a7, a8))
	(* cas ou (v $<=$ e3) *)
	| Noeud3(e1, e2, e3, Noeud2(e4, e5, a1, a2, a3), Noeud1(e6, a4, a5), Noeud1(e7, a6, a7), e8) when ((v $<$ e3) || (v $=$ e3)) -> Noeud3(e5, e6, e3, Noeud1(e4, a1, a2), Noeud1(e1, a3, a4), Noeud2(e2, e7, a5, a6, a7), e8)
	|	Noeud3(e1, e2, e3, Noeud3(e4, e5, e6, a1, a2, a3, a4), Noeud1(e7, a5, a6), Noeud1(e8, a7, a8), e9) when ((v $<$ e3) || (v $=$ e3)) -> Noeud3(e6, e7, e3, Noeud2(e4, e5, a1, a2, a3), Noeud1(e1, a4, a5), Noeud2(e2, e8, a6, a7, a8), e9)
	| Noeud3(e1, e2, e3, e4, Noeud2(e5, e6, a1, a2, a3), Noeud1(e7, a4, a5), e8) when ((v $<$ e3) || (v $=$ e3)) -> Noeud3(e1, e6, e3, e4, Noeud1(e5, a1, a2), Noeud2(e2, e7, a3, a4, a5), e8)
	| Noeud3(e1, e2, e3, e4, Noeud3(e5, e6, e7, a1, a2, a3, a4), Noeud1(e8, a5, a6), e9) when ((v $<$ e3) || (v $=$ e3)) ->  Noeud3(e1, e7, e3, e4, Noeud2(e5, e6, a1, a2, a3), Noeud2(e2, e8, a4, a5, a6), e9)
	| Noeud3(e1, e2, e3, e4, e5, Noeud1(e6, a1, a2), Noeud2(e7, e8, a3, a4, a5)) when ((v $<$ e3) || (v $=$ e3)) -> Noeud3(e1, e2, e7, e4, e5, Noeud2(e6, e3, a1, a2, a3), Noeud1(e8, a4, a5))
	| Noeud3(e1, e2, e3, e4, e5, Noeud1(e6, a1, a2), Noeud3(e7, e8, e9, a3, a4, a5, a6)) when ((v $<$ e3) || (v $=$ e3)) -> Noeud3(e1, e2, e7, e4, e5, Noeud2(e6, e3, a1, a2, a3), Noeud2(e8, e9, a4, a5, a6))
	(* cas ou (v $>$ e3) *)
	| Noeud3(e1, e2, e3,  Noeud2(e4, e5, a1, a2, a3), Noeud1(e6, a4, a5), Noeud1(e7, a6, a7), Noeud1(e8, a8, a9)) when (v $>$ e3) -> Noeud3(e5, e6, e7,  Noeud1(e4, a1, a2), Noeud1(e1, a3, a4), Noeud1(e2, a5, a6), Noeud2(e3, e8, a7, a8, a9))
	| Noeud3(e1, e2, e3,  Noeud3(e4, e5, e6, a1, a2, a3, a4), Noeud1(e7, a5, a6), Noeud1(e8, a7, a8), Noeud1(e9, a9, a10)) when (v $>$ e3) -> Noeud3(e6, e7, e8,  Noeud2(e4, e5, a1, a2, a3), Noeud1(e1, a4, a5), Noeud1(e2, a6, a7), Noeud2(e3, e9, a8, a9, a10))
	| Noeud3(e1, e2, e3, e4, Noeud2(e5, e6, a1, a2, a3), Noeud1(e7, a4, a5), Noeud1(e8, a6, a7)) when (v $>$ e3) -> Noeud3(e1, e6, e7, e4, Noeud1(e5, a1, a2), Noeud1(e2, a3, a4), Noeud2(e3, e8, a5, a6, a7))
	| Noeud3(e1, e2, e3, e4, Noeud3(e5, e6, e7, a1, a2, a3, a4), Noeud1(e8, a5, a6), Noeud1(e9, a7, a8)) when (v $>$ e3) -> Noeud3(e1, e7, e8, e4, Noeud2(e5, e6, a1, a2, a3), Noeud1(e2, a4, a5), Noeud2(e3, e9, a6, a7, a8))
	| Noeud3(e1, e2, e3, e4, e5, Noeud2(e6, e7, a1, a2, a3), Noeud1(e8, a4, a5)) when (v $>$ e3) -> Noeud3(e1, e2, e7, e4, e5, Noeud1(e6, a1, a2), Noeud2(e3, e8, a3, a4, a5))
	| Noeud3(e1, e2, e3, e4, e5, Noeud3(e6, e7, e8, a1, a2, a3, a4), Noeud1(e9, a5, a6)) when (v $>$ e3) -> Noeud3(e1, e2, e8, e4, e5, Noeud2(e6, e7, a1, a2, a3), Noeud2(e3, e8, a4, a5, a6))
	(* cas particulier ou tous les fils sont Noeud1 *)
	| Noeud2(e1, e2, Noeud1(e3, a1, a2), Noeud1(e4, a3, a4), Noeud1(e5, a5, a6)) when ((v $>$ e1) || (v $=$ e1)) -> Noeud1(e1, Noeud1(e3, a1, a2), Noeud3(e4, e2, e5, a3, a4, a5, a6))
	| Noeud2(e1, e2, Noeud1(e3, a1, a2), Noeud1(e4, a3, a4), Noeud1(e5, a5, a6)) when ((v $<$ e2) || (v $=$ e2)) -> Noeud1(e2, Noeud3(e3, e1, e4, a1, a2, a3, a4), Noeud1(e5, a5, a6))
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud1(e6, a5, a6), Noeud1(e7, a7, a8)) when ((v $<$ e2) || (v $=$ e2)) -> Noeud2(e2, e3, Noeud3(e4, e1, e5, a1, a2, a3, a4), Noeud1(e6 , a5, a6), Noeud1(e7, a7, a8))
	| Noeud3(e1, e2, e3, Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud1(e6, a5, a6), Noeud1(e7, a7, a8)) when (v $>$ e2) -> Noeud2(e1, e2,Noeud1(e4, a1, a2), Noeud1(e5, a3, a4), Noeud3(e6, e3, e7, a5, a6, a7, a8))
	| a -> a;;

(* fonction de suppression d'un element v dans un arbre234 *)
let supp_ab234 v a =
	let rec supp_aux = function
		| Vide -> Vide
		(* cas ou on a trouver la cible donc descente *)
		(* Noeud2 avec r = v *)
		| Noeud2(r, t, (Noeud1(_, _, _) as a1), a2, a3) when r $=$ v -> supp_aux (equilibrer_supp v (Noeud2(r, t, a1, a2, a3)))
		| Noeud2(r, t, Noeud2(e1, e2, a11, a12, a13), a2, a3) when r $=$ v -> Noeud2(e2, t, supp_aux (Noeud2(e1, r, a11, a12, a13)), a2, a3)
		| Noeud2(r, t, Noeud3(e1, e2, e3, a11, a12, a13, a14), a2, a3) when r $=$ v -> Noeud2(e3, t, supp_aux (Noeud3(e1, e2, r, a11, a12, a13, a14)), a2, a3)
		(* t = v *)
		| Noeud2(r, t, a1, (Noeud1(_, _, _) as a2), a3) when t $=$ v -> supp_aux (equilibrer_supp v (Noeud2(r, t, a1, a2, a3)))
		| Noeud2(r, t, a1, Noeud2(e1, e2, a11, a12, a13), a3) when t $=$ v -> Noeud2(r, e2, a1, supp_aux (Noeud2(e1, t, a11, a12, a13)), a3)
		| Noeud2(r, t, a1, Noeud3(e1, e2, e3, a11, a12, a13, a14), a3) when t $=$ v -> Noeud2(r, e3, a1, supp_aux (Noeud3(e1, e2, t, a11, a12, a13, a14)), a3)
		(* Noeud3 avec r = v *)
		| Noeud3(r, t, y, Noeud1(e1, a11, a12), a2, a3, a4) when r $=$ v -> supp_aux (equilibrer_supp v (Noeud3(r, t, y, Noeud1(e1, a11, a12), a2, a3, a4)))
		| Noeud3(r, t, y, Noeud2(e1, e2, a11, a12, a13), a2, a3, a4) when r $=$ v -> Noeud3(e2, t, y, supp_aux (Noeud2(e1, r, a11, a12, a13)), a2, a3, a4)
  	| Noeud3(r, t, y, Noeud3(e1, e2, e3, a11, a12, a13, a14), a2, a3, a4) when r $=$ v -> Noeud3(e3, t, y, supp_aux (Noeud3(e1, e2, r, a11, a12, a13, a14)), a2, a3, a4)
		(* t = v *)
		| Noeud3(r, t, y, a1, Noeud1(e1, a11, a12), a3, a4) when t $=$ v -> supp_aux (equilibrer_supp v (Noeud3(r, t, y, a1, Noeud1(e1, a11, a12), a3, a4)))
		| Noeud3(r, t, y, a1, Noeud2(e1, e2, a11, a12, a13), a3, a4) when t $=$ v -> Noeud3(r, e2, y, a1, supp_aux (Noeud2(e1, t, a11, a12, a13)), a3, a4)
  	| Noeud3(r, t, y, a1, Noeud3(e1, e2, e3, a11, a12, a13, a14), a3, a4) when t $=$ v -> Noeud3(r, e3, y, a1, supp_aux (Noeud3(e1, e2, t, a11, a12, a13, a14)), a3, a4)
		(* y = v *)
		| Noeud3(r, t, y, a1, a2, Noeud1(e1, a11, a12), a4) when y $=$ v -> supp_aux (equilibrer_supp v (Noeud3(r, t, y, a1, a2, Noeud1(e1, a11, a12), a4)))
		| Noeud3(r, t, y, a1, a2, Noeud2(e1, e2, a11, a12, a13), a4) when y $=$ v -> Noeud3(r, t, e2, a1, a2, supp_aux (Noeud2(e1, y, a11, a12, a13)), a4)
		| Noeud3(r, t, y, a1, a2, Noeud3(e1, e2, e3, a11, a12, a13, a14), a4) when y $=$ v ->  Noeud3(r, t, e3, a1, a2, supp_aux (Noeud3(e1, e2, y, a11, a12, a13, a14)), a4)
		(* cas particulier si la racine est Noeud1 avec e1 = v *)
		| Noeud1(e1, Noeud2(e2, e3, a11, a12, a13), a2) when (e1 $=$ v)-> Noeud1(e3, supp_aux (Noeud2(e2, e1, a11, a12, a13)), a2)
		(* cas supp *)
  	| Noeud2(r, t, Vide, Vide, Vide) when r $=$ v -> Noeud1(t, Vide, Vide)
		| Noeud2(r, t, Vide, Vide, Vide) when t $=$ v -> Noeud1(r, Vide, Vide)
  	| Noeud3(r, t, y, Vide, Vide, Vide, Vide) when r $=$ v -> Noeud2(t, y, Vide, Vide, Vide)
		| Noeud3(r, t, y, Vide, Vide, Vide, Vide) when t $=$ v -> Noeud2(r, y, Vide, Vide, Vide)
		| Noeud3(r, t, y, Vide, Vide, Vide, Vide) when y $=$ v -> Noeud2(r, t, Vide, Vide, Vide)
		(* cas ou on l'a pas encore trouve  *)
		(* descente dans la bonne branche avec equilibrage *)
		(* cas ou on trouve pas V *)
		| Noeud2(r, t, Vide, Vide, Vide) -> Noeud2(r, t, Vide, Vide, Vide)
		| Noeud3(r, t, y, Vide, Vide, Vide, Vide) -> Noeud3(r, t, y, Vide, Vide, Vide, Vide)
		(* retrait de l'equilibrage car doit deja etre quilibrer *)
		| Noeud1(e1, ag, ad) when v $<$ e1 -> Noeud1(e1, supp_aux ag, ad)
		| Noeud1(e1, ag, ad) when v $>$ e1 -> Noeud1(e1, ag, supp_aux ad)
		(* tour de descente *)
		| Noeud2(r , y, (((Noeud2(_, _, _, _, _)) | (Noeud3(_, _, _, _, _, _, _))) as ag), am, ad) when v $<$ r -> Noeud2(r , y, supp_aux ag, am, ad)
  	| Noeud2(y , r, ag, am, (((Noeud2(_, _, _, _, _)) | (Noeud3(_,_,_, _,_,_,_))) as ad)) when v $>$ r -> equilibrer_supp v (Noeud2(y , r, ag, am, supp_aux ad))
  	| Noeud2(y , r, ag, (((Noeud2(_, _, _, _, _)) | (Noeud3(_,_,_, _,_,_,_))) as am), ad) -> equilibrer_supp v (Noeud2(y , r, ag, supp_aux am, ad))
		(* tour d'equilibrage *)
  	| Noeud2(y , r, ag, am, ad) -> supp_aux (equilibrer_supp v (Noeud2(y , r, ag, am, ad)))
		(* tour de descente *)
  	| Noeud3(r, x, y, (((Noeud2(_, _, _, _, _)) | (Noeud3(_,_,_, _,_,_,_))) as ag), amg, amd, ad) when v $<$ r -> equilibrer_supp v (Noeud3(r , x, y, supp_aux ag, amg, amd, ad))
  	| Noeud3(x, r, y, ag, (((Noeud2(_, _, _, _, _)) | (Noeud3(_,_,_, _,_,_,_))) as amg), amd, ad) when v $<$ r -> equilibrer_supp v (Noeud3(x, r, y, ag, supp_aux amg, amd, ad))
  	| Noeud3(x , y, r,ag, amg, (((Noeud2(_, _, _, _, _)) | (Noeud3(_,_,_, _,_,_,_))) as amd), ad) when v $<$ r -> equilibrer_supp v (Noeud3(x , y, r,ag, amg, supp_aux amd, ad))
  	| Noeud3(x , y, r, ag, amg, amd, (((Noeud2(_, _, _, _, _)) | (Noeud3(_,_,_, _,_,_,_))) as ad)) -> equilibrer_supp v (Noeud3(x , y, r, ag, amg, amd, supp_aux ad))
		(* tour d'equilibrage *)
  	| Noeud3(x , y, r, ag, amg, amd, ad) -> supp_aux (equilibrer_supp v (Noeud3(x, y, r, ag, amg, amd, ad)))
		(* cas a mettre en dernier car extreme ! *)
		(* dans la racine avec uniquement hauteur 1 ! *)
  	| Noeud1(r, Vide, Vide) when r $=$ v -> Vide
		| Noeud1(r, Vide, Vide) -> Noeud1(r, Vide, Vide)
	(* ajout derniere minute le 9 mai a 22h02 de equilibrer_supp v dans la ligne si dessous *)
	(* pour fix bug supp_ab234 5 (Noeud2                                  *)
 	(* (11, 26, Noeud1 (5, Vide, Vide), Noeud2 (12, 25, Vide, Vide, Vide), *)
 	(*  Noeud1 (27, Vide, Vide)))                                          *)
	in supp_aux (equilibrer_supp v (equilibrer_supp_rac v a));;

(* transforme un arbre 234 en liste *)
let rec ab234_to_list = function
	| Vide -> []
	| Noeud1(e1, a1, a2) -> e1:: (ab234_to_list a1) @ (ab234_to_list a2)
	| Noeud2(e1, e2, a1, a2, a3) -> e1::e2:: (ab234_to_list a1) @ (ab234_to_list a2) @ (ab234_to_list a3)
	| Noeud3(e1, e2, e3, a1, a2, a3, a4) -> e1::e2::e3:: (ab234_to_list a1) @ (ab234_to_list a2) @ (ab234_to_list a3) @ (ab234_to_list a4);;

(* Ajout de tout elements d'une list dans un arbre 234 *)
let add_list xs ab =
	List.fold_left (fun acc x -> inserer x acc) ab xs;;
(* supprime tout elements d'une list dans un arbre 234 *)
let supp_list xs ab =
	List.fold_left (fun acc x -> supp_ab234 x acc) ab xs;;

(* Cree un arbre 234 a partir d'une list *)
let from_list xs = add_list xs Vide;;

(* fonction qui fait l'union de deux arbres *)
let union a1 a2 =
	add_list (ab234_to_list a1) a2;;

(* fonction qui fait l'intersection de deux arbres *)
let intersection a1 a2 =
	let rec aux acc a = 
		match a with
		| [] -> acc
		| v::a when (est_dans v a2) = true -> aux (inserer v acc) a
		| v::a -> aux acc a
		in aux Vide (ab234_to_list a1);;

(* fonction qui fait la difference de deux arbres a1\a2 *)
let dif a1 a2 =
	supp_list (ab234_to_list a2) a1;;

(* fonction qui fait la difference symetrique de deux arbres *)
let delta a1 a2 = 
	dif (union a1 a2) (intersection a1 a2);;

(* fonction qui teste l'egalité de deux arbres *)
let egalite a1 a2 =
	let rec aux a ab = 
		match a with
		| [] when (ab = Vide) -> true
		| v::a when (est_dans v a2) = true -> aux a (supp_ab234 v ab)
		| _ -> false
		in aux (ab234_to_list a1) a2;;

(* fonction qui teste l'inclusion d'un arbres dans un autre*)
let inclusion a1 a2 =
	let rec aux a = 
		match a with
		| [] -> true
		| v::a when (est_dans v a2) = true -> aux a
		| _ -> false
		in aux (ab234_to_list a1);;

(* ------------- *)
(* fonction equivalente a List.fold_(left | right) *)
let left_fold f init ab = 
	let rec aux acc = function
	| [] -> acc
	| a::rest -> match a with
		| Vide -> aux acc rest
  	| Noeud1(e1, a1, a2) -> aux (f acc e1) (a1::a2::rest)
  	| Noeud2(e1, e2, a1, a2, a3) -> aux (f(f acc e1)e2) (a1::a2::a3::rest)
  	| Noeud3(e1, e2, e3, a1, a2, a3, a4) -> aux (f(f(f acc e1)e2)e3) (a1::a2::a3::a4::rest)
	in aux init [ab];;

let right_fold f init ab =
	let rec aux acc = function
	| [] -> acc
	| a::rest -> match a with
		| Vide -> aux acc rest
  	| Noeud1(e1, a1, a2) -> aux (f e1 acc) (a1::a2::rest)
  	| Noeud2(e1, e2, a1, a2, a3) -> aux (f e1(f e2 acc)) (a1::a2::a3::rest)
  	| Noeud3(e1, e2, e3, a1, a2, a3, a4) -> aux (f e1(f e2(f e3 acc))) (a1::a2::a3::a4::rest)
	in aux init [ab];;
(* --------------------------------------- *)
(* fonction pour le cardinal d'un ensemble *)
let inc acc = function
	| _ -> acc + 1;;

(* fonction permettant de calculer le cardinal d'un ensemble *)
let cardinal ab =
	left_fold inc 0 ab;;
(* --------------------------------------- *)
(* fonction pour la fonction de séparation *)
let fst1 (a,b,c) = a;;
let mdl (a,b,c) = b;;
let lst (a,b,c) = c;;

let separ acc = function
	| a when a <= mdl(acc) -> (inserer a (fst1 acc), mdl(acc), (lst acc))
	| a -> ((fst1 acc), mdl(acc), inserer a (lst acc));;

(* fonction séparant un arbre en 2 autour d'un valeur x *)
let separation x ab = 
	let aux acc =
  	((fst1 acc), (lst acc))
	in aux (left_fold separ (Vide,x,Vide) ab );;
(* ------------------------------------- *)
(* fonction pour la fonction de filtrage *)
let fil f acc = function
	| a when (f a) = true -> inserer a acc
	| a -> acc;;

(* fonction a utiliser pour le filtrage *)
let filter f ab =
	left_fold (fil f) Vide ab;;

	
