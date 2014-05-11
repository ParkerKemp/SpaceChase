(* ::Package:: *)

BeginPackage["Particles`", {"Environment`", "Geometry`"}]

initParticleEngine
createParticles
destroyParticles
incrementParticles

numParticles
parPos
parRot

Begin["Private`"]

initParticleEngine[] := Module[
	{},
	numParticles = 0;
	parPos = {};
	parVec = {};
	parRot = {};
	parAng = {};
]

incrementParticles[] := Module[
	{i},
	For[i = 1, i <= numParticles, i++,
		parPos[[i]] = Mod[parPos[[i]] + parVec[[i]], environmentSize];
		parRot[[i]] += parAng[[i]];
	]
]

destroyParticles[] := Module[
	{},
	numParticles = 0;
]

createParticles[pos_, vel_] := Module[
	{i, ang = angleFromVector[vel], speed = vectorMagnitude[vel]},
	numParticles = 0;
	parPos = {};
	parVec = {};
	parRot = {};
	parAng = {};
	
	For[i = 1, i < 10, i++,
		newParticle[pos, ang, speed]
	];
]

newParticle[pos_, ang_, speed_] := Module[
	{},
	numParticles++;
	AppendTo[parPos, pos];
	AppendTo[parVec, vectorVarianceFromAngle[ang, 0.2] * speed * variance[0.25]];
	AppendTo[parRot, 0];
	AppendTo[parAng, RandomReal[] * 2\[Pi]];
]

variance[var_] := RandomReal[] * 2 var + 1 - var

End[]

EndPackage[]
