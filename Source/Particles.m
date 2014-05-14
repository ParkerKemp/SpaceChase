(* ::Package:: *)

BeginPackage["Particles`", {"Environment`", "Geometry`"}]

initParticleEngine::usage="initParticleEngine[] initializes all attributes associated with particles."
createParticles::usage="createParticles[position, velocity] creates a list of particles starting at position and with vectors varying around velocity."
destroyParticles::usage="destroyParticles[] destroys all particles."
incrementParticles::usage="incrementParticles[] increments all particle attributes for one frame."

numParticles
parPos
parRot

Begin["Private`"]

initParticleEngine[] := Module[
	{},
	(*Initialize all attributes with default values.*)

	numParticles = 0;
	parPos = {};
	parVec = {};
	parRot = {};
	parAng = {};
]

incrementParticles[] := Module[
	{i},
	(*Increment all particle attributes for one frame.*)

	For[i = 1, i <= numParticles, i++,
		parPos[[i]] = Mod[parPos[[i]] + parVec[[i]], environmentSize];
		parRot[[i]] += parAng[[i]];
	]
]

destroyParticles[] := Module[
	{},
	(*"Destroy" all particles (for simplicity, we just
		reset numParticles so that For loops will not
		execute.*)

	numParticles = 0;
]

createParticles[pos_, vel_] := Module[
	{i, ang = angleFromVector[vel], speed = vectorMagnitude[vel]},
	(*Create 10 particles at pos, with varying 
		vectors based on vel.*)

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
	(*Create a new particle with a varied vector
		based on ang (direction) and speed (magnitude).*)

	numParticles++;
	AppendTo[parPos, pos];
	AppendTo[parVec, vectorVarianceFromAngle[ang, \[Pi]/8] * speed * variance[0.25]];
	AppendTo[parRot, 0];
	AppendTo[parAng, RandomReal[] * \[Pi]/2];
]

variance[var_] := RandomReal[] * 2 var + 1 - var
	(*Return (1 +/- var) *)

End[]

EndPackage[]
