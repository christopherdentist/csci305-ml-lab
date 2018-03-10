(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Christopher McCabe
* christopherdentist@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)
datatype 'a set = Empty | Set of 'a * 'a set;  (* Datatype 'set' *)

(* Boolean, true if the first variable is a member of the second variable. The second variable must be a set. *)
fun isMember e Empty = false  (* Base case for recursion; if the list of entries to check is empty, then the value must not be in the set. *)
  | isMember e (Set s) = if (#1 s) = e then true else isMember e (#2 s);
  
(* Converts a list to a set. Recursively checks if the lone value is in the set yet, and if not, then adds it. *)
fun list2Set [] = Empty
  | list2Set (first::rest) = 
    let
      val x = list2Set rest
    in
      if isMember first x then x else Set(first, x)
    end;

(* Converts a set to a list, recursively adding the lone value to the left of the list being built. *)
fun set2List Empty = []
  | set2List (Set s) = (#1 s)::(set2List (#2 s));
  
(* Find the union between two sets. Converts to a list, concatenates them together, and then converts the resulting list back into a set. *)  
fun union set1 set2 = 
  let
    val l1 = set2List set1
    val l2 = set2List set2
  in
    list2Set (l1@l2)
  end;
  
(* Find the intersect between two sets. Checks if both sets possess the item before adding them to the resulting set. *)
fun intersect set1 Empty = Empty
  | intersect set1 (Set set2) = if isMember (#1 set2) set1 then Set((#1 set2), intersect set1 (#2 set2)) else intersect set1 (#2 set2);
  
(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9]

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
