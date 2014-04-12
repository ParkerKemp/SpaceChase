(* ::Package:: *)

(* Name of package MUST end with ` *)
BeginPackage["XSquared`"]

(* Public declaration of function called xSquared. 
Also, usage defines what will be displayed on ?xSquared *)
xSquared

Begin["Private`"]
(* Everything between Begin[] and End[] will be invisible to other 
packages (however, the code will still execute) *)

(* Definition of what xSquared actually 
does. This can be any kind of function. *)
xSquared[x_]=x^2

End[]

EndPackage[]




