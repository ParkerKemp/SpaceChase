(* ::Package:: *)

BeginPackage["Parallel`"]

startParallel

Begin["Private`"]

startParallel[] := Module[
	{},
	LaunchKernels[1];
	RunScheduledTask[Parallel`Developer`QueueRun[], 0.001]
]

End[]

EndPackage[]
