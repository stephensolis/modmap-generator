(* ::Package:: *)

BeginPackage["MDS`", {"RLink`"}]

MDS::usage = 
	"MDS[delta, dim] performs MDS in the given number of dimensions on the given distance matrix.\nMDS[delta, dim, accuracy] does the same, with a given argument to N."
MDS::dimerr = 
	"Not enough positive eigenvalues for this dimension"
	
RMDS::usage = 
	"RMDS[delta, dim] calls the cmdscale function in R to perform classical MDS on the given distance matrix and the given number of dimensions."


Begin["`Private`"]

MDS[delta_List?MatrixQ, dim_Integer?Positive, accuracy_Integer: 15]:=
	Module[{n, deltasq, deltatotals, sumOfDelta, bMatr, eigenvals, eigenvecs, solpts},
		n = Length[delta];
		
		deltasq = delta^2;
		deltatotals = Total[deltasq];
		sumOfDelta = Total[deltasq, 2];
		
		bMatr = -0.5*(deltasq - ConstantArray[deltatotals/n, n] - (ConstantArray[#, n] & /@ (deltatotals/n)) + ConstantArray[sumOfDelta/(n^2), {n, n}]);

		{eigenvals, eigenvecs} = Eigensystem[N[bMatr, accuracy], dim];

		If[!VectorQ[eigenvals, Positive],
			Message[MDS::dimerr];
			Return[$Failed];
		];

		solpts = Transpose[eigenvecs].Sqrt[DiagonalMatrix[eigenvals]];
		Return[solpts];
	];

RMDS[delta_List?MatrixQ, dim_Integer?Positive]:=
	Module[{result},
		InstallR[];
		
		RSet["delta", delta];
		RSet["dim", dim];
		
		result = REvaluate["cmdscale(delta, k=dim)"];
		
		Return[First@result];
	];


End[]
EndPackage[]
