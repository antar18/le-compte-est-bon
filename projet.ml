



(*definition de type expression *)

type expression = Valeur of int | Plus of expression*expression | Moins of expression*expression | Mult of expression*expression  | Div of expression*expression ;;



(*definition de type nombre *)

type nombre = { valeur: int; exp:expression };;



(*definition des différentes operations *)

let nombre a = { valeur = a ; exp = Valeur a };;


let mult(a,b) = {valeur=(a.valeur*b.valeur); exp =Mult(a.exp, b.exp) };;
let moins (a,b)= {valeur=(a.valeur-b.valeur); exp = Moins(a.exp,b.exp)};;
let plus (a,b)= {valeur=(a.valeur+ b.valeur); exp =Plus(a.exp, b.exp);};; 
let div (a,b)= {valeur= a.valeur /  b.valeur; exp =Div(a.exp, b.exp)};;



(*un ptit test *)

let a = nombre 5;;
let b = nombre 7;;
let c = nombre 1;;
let d = nombre 25;;
let e = nombre 2;;




let res = mult(moins(mult(a,b),c),moins(d,e));;

let nb = {valeur = 782; exp = Mult(Moins(Mult(Valeur 5, Valeur 7),
                                         Valeur 1),
                                   Moins(Valeur 25, Valeur 2))};;




(*methode permattant d'afficher un nombre *)

let rec afficher exp =
  match exp with 
    | Valeur a -> string_of_int a
    | Mult(a,b) -> "("^ afficher a ^"*"^ afficher b ^")"
    | Div(a,b) -> "("^ afficher a ^"/"^ afficher b ^")"
    | Plus(a,b) -> "("^ afficher a ^"+"^ afficher b ^")"
    | Moins(a,b) -> "("^ afficher a ^ "-" ^ afficher b ^ ")";;




let nombre_to_string n = n.valeur , afficher n.exp ;;



nombre_to_string nb ;; 









(*---------------------------------------------------------------------------*)
(*------partie 01 :: construction de toutes les combainaisons possibles -----*)
(*---------------------------------------------------------------------------*)




(*****--01--creation de combainison de triplet à partir d'une liste******** *)
(*---------------------------------------------------------------------- *)




(* fonction qui donne tous les couples possible à partir d'une liste d'elements *)
let rec liste_couples l =
  match l with
    | [] -> []
    | h::t -> List.map (function x-> (h,x)) t @ liste_couples t;;



(*fonction pour construire une liste qui ne contient pas les elements d'un couple à partir d'une liste *)


let rec construire_liste (a,b) l =
  match l with 
    | [] -> []
    | h::t -> if ( a = h ) || ( b = h ) then (construire_liste (a,b) t )
        else h::( construire_liste (a,b) t ) ;;


construire_liste (1,2) [1;2;3;4];;




let rec combinaison l =
  let l' = liste_couples l in
    List.map ( fun (a,b) -> a,b,construire_liste (a,b) l) l' ;;

combinaison [1;2;3;4];;






(*fonction pour filtrer une liste avec un predicat *)

let rec filter pred l = 
  match l with 
    | [] -> [] 
    | h::t -> if (pred h ) then h::(filter pred t)
        else filter pred t ;; 




(*methode qui calcul le nombre dapparition d'un element dans une liste*)

let rec nb_fois n l = 
  match l with 
    | [] -> 0
    | h::t -> if h = n then 1 + ( nb_fois n t ) else ( nb_fois n t );;





(* methode qui teste si un ensemble est inclus dans un autre ensemble *)
let rec est_inclus_dans l l' =
  let t1 = List.length l
  in let t2 = List.length l'
  in 
    if t1 <= t2 then 
      match l with 
        | [] -> true 
        | h::t -> if (nb_fois h l ) <= (nb_fois h l') then est_inclus_dans t l'
            else 
              false 
    else false ;;

est_inclus_dans [1;2;3] [1];;
est_inclus_dans  [1] [1;2;3];;







(*methode qui teste si deux ensembles sont egaux *)

let sont_egaux l l' = if (est_inclus_dans l l') && (est_inclus_dans l' l) then true else false ;;









let rec ensemble exp =
  match exp with
    | Valeur a -> [a]
    | Mult(a,b) -> (ensemble a )@(ensemble b)
    | Plus(a,b)-> (ensemble a )@(ensemble b)
    | Div(a,b)-> (ensemble a )@(ensemble b)
    | Moins(a,b)-> (ensemble a )@(ensemble b);;

let nb = {valeur = 782; exp = Mult(Moins(Mult(Valeur 5, Valeur 7),
                                         Valeur 1),
                                   Moins(Valeur 25, Valeur 2))};;


let e = nb.exp ;;

ensemble e ;;

let meme_nombre n1 n2 =
  if n1.valeur = n2.valeur then
    if sont_egaux (ensemble n1.exp ) (ensemble n2.exp) then true 
    else false 
  else false ;;


let n1={valeur = 1; exp = Div(Valeur 20,Valeur 2) } ;;
let n2={valeur = 1 ; exp = Moins(Valeur 2,Valeur 20) } ;;

meme_nombre  n1 n2;;



(* methode qui teste si un nombre est contenu dans une liste de nombre *)

let rec  contains e l = 
  match l with 
    | [] -> false 
    | h::t -> if meme_nombre e h then true 
        else contains e t ;; 

(* methode qui teste si un nombre est deja contenu dans un liste de liste de nombre *)

let contains_liste_liste e l = 
  let rec aux_1 l = match l with 
    | [] -> false
    | h::t -> let rec aux_2 a =
      match a with
        | [] -> aux_1 t
        | n::[] -> if (meme_nombre e n) then true else aux_1 t 
        | n::m -> if ( meme_nombre e n) then true else aux_2 m 
        in
          aux_2 h 
  in
    aux_1 l ;;


let a = {valeur = 1 ; exp = Valeur 1 };;

contains a [n1;n2];;












(* methode pour additioner deux nombres*)

let addition n1 n2 l =
  let a = n1.valeur and b = n2.valeur 
  in match a,b with 
    | (0,n) -> if n <= 0 || contains n2 l then [l] else [n2]::[l]
    | (n,0) -> if n <= 0 || contains n1 l then [l] else [n1]::[l]
    | (n,m) -> let c = { valeur = n + m ; exp = Plus(n1.exp,n2.exp)}
        in if contains c l || c.valeur <= 0 then [l]
          else [c]::[l];;









(*methdode pour soustrair edeux nombres *)

let soustraction n1 n2 l = 
  let a = n1.valeur and b = n2.valeur 
  in match a,b with 
    |(n,0) -> if n <= 0 then [l] else [n1]::[l]
    | (0,n) -> if n <= 0 then [n2]::[l] else [l]
    |(n,m) -> if n > m then let c = { valeur = (n-m); exp = Moins(n1.exp,n2.exp) }   
          in if (contains c l) || c.valeur <= 0 then [l]
            else [c]::[l] 
        else let c = { valeur = (m-n); exp = Moins(n2.exp,n1.exp) } in
            if (contains c l) || c.valeur <= 0 then [l]
            else [c]::[l] ;;




soustraction a b [n1;n2];;





(*methode pour faire la division *)

let division n1 n2 l = 
  let a = n1.valeur and b = n2.valeur 
  in match a,b with
    |(n,0) -> [l]
    |(0,n) -> [l]
    |(n,m) -> if n > m && n mod m = 0 then let c = { valeur = (n/m); exp = Div(n1.exp,n2.exp) } in
            if (contains c l) || c.valeur <= 0 then [l] else [c]::[l]
        else if m mod n = 0 then let c = { valeur = (m/n); exp = Div(n2.exp,n1.exp) }
          in if (contains c l ) || c.valeur <= 0 then [l] else [c]::[l]
        else [l];;

let d = {valeur=8 ; exp=Moins(Valeur 16,Valeur 8)} ;;










division a b [n1;n2];;



(*methode pour faire la multiplication*)


let multiplication n1 n2 l =
  let a = n1.valeur and b = n2.valeur 
  in match (a,b) with
    | (n,0) -> [l]
    | (0,m) -> [l]
    | (n,m) -> let c = { valeur = n * m ; exp = Mult(n1.exp,n2.exp) } in
          if c.valeur <= 0 || contains c l then [l]
          else [c]::[l] ;;




multiplication b a [n1;n2];;




(*construction des combinaisons possibles *)


(* à verifier *) 
let liste_flat l = List.flatten l ;;

(* methode pour obtenir toutes les combinaisons possibles *)

let rec all_combinaison l =
  let combinaison_triplet t = match t with 
    |(n,m,[])->  multiplication n m  []  @ addition n m []  @ division a b [] @ soustraction a b [] 
    |(n,m,q) -> all_combinaison ( liste_flat (multiplication n m q)) 
                @ all_combinaison (liste_flat ( soustraction n m q)) @ all_combinaison (liste_flat ( addition  n m q)) @ all_combinaison (liste_flat (division  n m q)) 
  in
  let l' = combinaison l 
  in 
  let rec aux c  = match c with 
    |[]->[]
    |h::t->(combinaison_triplet h) @ ( aux t )
  in aux l' ;;


let antar = [nombre 1 ; nombre 2 ; nombre 3];;



all_combinaison antar;;




(*methode pour filtrer une liste*)

let rec filtrer_liste l =
  match l with 
    | []->[] 
    | h::t-> if  (contains h t) then filtrer_liste t else 
          h::(filtrer_liste t);;

let rec liste_of_nombre l = match l with
  | [] -> []
  | h::t -> (nombre h)::(liste_of_nombre t);;

let combinaison_possible l = filtrer_liste ( liste_flat (all_combinaison (liste_of_nombre l)) ) ;;

combinaison_possible [1;2;3];;

(*methode pour afficher un nombre resultant *)
let  afficher_nombre n = "une valeur = "^(string_of_int n.valeur)^" correspond à l'expression "^(afficher n.exp)^"\n" ;;

(*methode pour afficher les nombres resultants *) 

let rec afficher_resultat l = match l with
  | [] -> ""
  | h::t -> (afficher_nombre h)^( afficher_resultat t);;



afficher_resultat ( combinaison_possible [1;2;3]);;

(*reponse 1ere partie *)

let reponse_1 l = afficher_resultat ( combinaison_possible l);;

reponse_1 [1;2;3];;






(*********************deuxieme partiee*********************)

(*methode qui retourne le plus proche nombre parmi deux *)

let plus_proche n n1 n2 =
  let a = n1.valeur and b = n2.valeur 
  in let res_n1 = abs (n - a ) and res_n2 = abs (n -b)
  in
    if res_n1 < res_n2 then n1 else n2;; 


(* methode qui retourne le plus proche nombre parmi une liste de nombre *)

let rec meilleure_approximation a l =

  List.fold_right ( fun x y -> plus_proche a x y ) l (List.hd l );;

(* methode qui retourne la meilleure approximation parmi une combinaison *)

exception Liste_vide;;

let la_meilleure_combinaison a l = 
  afficher_nombre ( meilleure_approximation a ( combinaison_possible l ));;


la_meilleure_combinaison 2000 [2; 4; 3; 7; 25];;




let reponse_2 a l = la_meilleure_combinaison a l ;;










(* ****troisieme partie ****** *)


(*methode pour qui verifier si on trouve une solution sinon elle throws une exeption *)

exception Solution_pas_trouve ;;

let solution_trouve a l = let b = meilleure_approximation a (liste_of_nombre l )in
    if a = b.valeur then b 
    else raise Solution_pas_trouve ;; 



let reponse_3 a l = afficher_nombre ( solution_trouve a l ) ;;

(*methode qui tronsforme un string en une liste de string *)
let rec liste_string str =
  let n = String.length str in 
    match n with 
      | 0 -> [] 
      | _ -> [(String.sub str 0 1 )]@(liste_string (String.sub str 1 (n-1)))  ;;


(*methode qui filtre une liste et retourne seulement une liste d'entier *)
let rec liste_int l = 
  match l with 
    | [] -> []
    | h::t -> try ([(int_of_string h)])@ (liste_int t) 
        with 
            _-> liste_int t ;;



(****le main du programme *)


exception Out_Of_Bound ;;
exception Liste_vide ;;
exception Valeur_invalide ;;


let saisie n =
  if String.length n = 1 then
    match n with
      | "1" -> 
          begin 
            print_string " veuillez entrer une liste d'entier ";
            print_newline();
            print_string " !! merci d'entrer 3 entier seulement !!";
            let liste = liste_int (liste_string (read_line())) in 
              if
                (List.length liste > 10) then reponse_1 liste
              else
                raise Out_Of_Bound

          end ;;
| "2" -> 
  begin
    print_string " veuillez entrer un entier cible ";
    let a = ( int_of_string (read_line())) in
      print_string " veuillez entrer une liste d'entiers : " in
let liste = list_int ( liste_string (read_line())) in
  if List.length liste < 10 then
    reponse_2 a  liste
  else 
    raise Out_Of_Bound

end ;;

| "3" -> 

  begin  
    print_string " veuillez entrer un entier cible ";
    let a = ( int_of_string (read_line())) in
      print_string " veuillez entrer une liste d'entiers " in
let liste = list_int ( liste_string (read_line())) in 
  if 
    List.length liste > 10 then raise Out_Of_Bound
  else reponse_3 a  liste

| "4" -> exit (0);;

else raise Valeur_invalide ;;




let main () = 
  let rec affichage () = 
    begin
      print_string "********************************** \n
a - afficher les combinaisons à partir d'une liste \n
b - afficher la meilleure approximation  \n
c -  peut_on obtenir le numero exact ? \n,
d - quitter ";
    end;
  in 

  let n = read_line() in
    try (saisie n ) ; affichage () 
    with
      | Out_Of_Bound -> print_string " liste invalide " ; affichage ();
      | Solution_pas_trouve -> print_string "solution pas trouvé"; affichage ();
      | _ -> print_string " saisie incorrecte "; affichage ();
in
  affichage ();;

main () ;;






exception Invalide;;
exception Out_Of_Bound;;

let saisie c  =
  if String.length c = 1
  then
    match c  with
      | "a" ->  begin  
          print_string " veuillez entrer une liste d'entier : ";
          print_newline();
          print_string " !! merci d'entrer 3 entier seulement !!";
          let l = liste_int(liste_string (read_line())) in
            if List.length l >9 then raise Out_Of_Bound
            else reponse_1  l  
        end
      | "b" -> begin
          print_string "veuillez entrer un entier :  " ;
          let n = (int_of_string (read_line())) in
            print_string "veuillez entrer une liste d'entiers  " ;
            let l = liste_int (liste_string(read_line())) in
              reponse_2 n l
        end
      |"c" -> begin
          print_string " veuillez entrer un entier :  " ;
          let n = (int_of_string(read_line())) in
            print_string "veuillez entrer une liste d'entiers :  " ;
            let l = liste_int(liste_string(read_line())) in
              reponse_3 n l
        end
      | "e" -> exit (0)
  else raise Invalide;;


let  main () = 
  let rec affichage() =
    print_string "******Le Compte est Bon ***** \n 
a- afficher les combinaisons         \n
b - obtenir la meilleure combinaison  \n
c - peut on obtenir le nombre exact ?  \n
e - exit                                \n ";
    let n = read_line()
    in
      try (saisie n) ; affichage() with 
        |Solution_pas_trouve ->print_string "pas de resultat exact \n";affichage()
        |Out_Of_Bound->  print_string "\n veuillez saisir une liste qui depasse pas 10 elements \n";affichage()   
        | _ ->print_string"saisie invalide \n" ; affichage()

  in affichage()  

;;


main ();;





