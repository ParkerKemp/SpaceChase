(* ::Package:: *)

BeginPackage["Geometry`", {"Environment`", "Player`"}]

vectorToPoint::usage="vectorToPoint[start, end] returns a vector along the shortest path from start to end."
unitVector::usage="unitVector[vec] returns a unit vector in the same direction as vec."
unitVectorFromAngle::usage="unitVectorFromAngle[angle] returns a unit vector in the absolute direction of angle."
vectorMagnitude::usage="vectorMagnitude[vector] returns the length of vector."
vectorVarianceFromAngle::usage="vectorVarianceFromAngle[angle, variance] returns a random vector within variance (radians) of the direction of angle."
minDistance::usage="minDistance[p1, p2] returns the minimum distance between p1 and p2."
checkCollision::usage="checkCollision[alienPos, numAliens] returns the index of the alien with which the player is colliding, or returns 0 if there is no collision."
angleFromVector::usage="angleFromVector[vector] returns the angle in radians matching the absolute direction of vector."

Begin["Private`"]

vectorToPoint[start_,end_]:=Module[
	{diffx,diffy,vec={0,0}},
	(*Return a vector pointing from start to end (given the 
		wrapped-around nature of our environment.*)

	diffx=Mod[end[[1]]-start[[1]],environmentSize[[1]]];
	diffy=Mod[end[[2]]-start[[2]],environmentSize[[2]]];
	(*Using modulo gives us the vector components if 
		one were to travel positively on the x and y axes.*)
	
	(*If a component is greater than half of the environment
		length on that axis then we subtract the environment length,
		giving the smaller negative component. Otherwise, we pass along
		the original component.*)
	If[diffx<environmentSize[[1]]/2,
		vec[[1]]=diffx,
		vec[[1]]=diffx-environmentSize[[1]]
	];
	If[diffy<environmentSize[[2]]/2,
		vec[[2]]=diffy,
		vec[[2]]=diffy-environmentSize[[2]]
	];
	Return[vec]
]

angleFromVector[vec_]:=Module[
	{unit=unitVector[vec]},
	(*Return the angle (in radians) corresponding to a vector.*)

	(*We use ArcTan; however, it only has a domain of (-\[Pi]/2, \[Pi]/2) while
		we need domain (0, 2\[Pi]). It turns out that simply adding \[Pi] to the result
		in case of a negative x component returns a correct angle.*)
	If[unit[[1]] < 0,
		Return[N[ArcTan[unit[[2]]/unit[[1]]]]+ \[Pi]],
	];
	If[unit[[1]] > 0,
		Return[N[ArcTan[unit[[2]]/unit[[1]]]]]
	];

	(*Special case: x component is zero. Angle is either \[Pi]/2 or 3\[Pi]/2,
		depending on the sign of the y component.*)
	If[unit[[2]] > 0,
		Return[N[\[Pi]/2]],
		Return[N[3 \[Pi]/2]]
	];
]

unitVectorFromAngle[ang_] := {Cos[ang], Sin[ang]}
	(*Simple conversion from angle to unit vector.*)

unitVector[v_] := Module[
	{},
	(*Take a vector and return the unit vector in the same direction.*)

	(*Special case: zero vector has no magnitude and will 
		result in divide-by-zero if we proceed.*)
	If[v == {0,0},
		Return[v]
	];
	Return[{v[[1]]/vectorMagnitude[v],v[[2]]/vectorMagnitude[v]}]
]

vectorVarianceFromAngle[ang_, var_] := Module[
	{newAngle},
	(*Return a vector with varied direction from the given angle.*)

	(*newAngle is ang +/- var*)
	newAngle = RandomReal[] * 2 var + ang - var;
	Return[unitVectorFromAngle[newAngle]];
]

vectorMagnitude[vec_] := Sqrt[vec[[1]]^2 + vec[[2]]^2]
	(*Length of vector.*)

minDistance[p1_, p2_] := Module[
	{x, y},
	(*Get the minimum distance between p1 and p2 (given
		our wrapped-around environment).*)

	(*Find minimum distance along each axis, then
		apply pythagorean theorem.*)
	x = Abs[p1[[1]] - p2[[1]]];
	x = Min[x, environmentSize[[1]] - x];
	y = Abs[p1[[2]] - p2[[2]]];
	y = Min[y, environmentSize[[2]] - y];
	Return[Sqrt[x^2+y^2]]
]

checkCollision[alienPos_, numAliens_] := Module[
	{i},
	(*Check whether the player collides with an alien,
		return the alien index if so, or 0 if not.*)

	For[i = 1, i <= numAliens, i++,
		If[EuclideanDistance[alienPos[[i]], playerPos] < 30,
			Return[i]
		]
	];
	Return[0]
]

End[]

EndPackage[]



