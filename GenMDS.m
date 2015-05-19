(* ::Package:: *)

BeginPackage["GenMDS`"]

MDS::usage = 
	"MDS[delta, dim] performs MDS in 2 or 3 dimensions on the given distance matrix.\nMDS[delta, dim, accuracy] does the same, with a given argument to N."
MDS::dimerr = 
	"Not enough positive eigenvalues for this dimension"


Begin["`Private`"]

MDS[delta_List?MatrixQ, dim_Integer?Positive, accuracy_Integer: 20]:=
	Module[{n, deltasq, deltatotals, sumOfDelta, bMatr, eigenvals, eigenvecs, solpts},
		n = Length[delta];
		
		deltasq = delta^2;
		deltatotals = Total[deltasq]/n;
		sumOfDelta = Total[deltasq, 2]/(n^2);
		
		bMatr = -0.5*(deltasq - ConstantArray[deltatotals, n] - (ConstantArray[#, n] & /@ deltatotals) + ConstantArray[sumOfDelta, {n, n}]);

		{eigenvals, eigenvecs} = Eigensystem[N[bMatr, accuracy], dim];

		If[!VectorQ[eigenvals, Positive],
			Message[MDS::dimerr];
			Return[$Failed];
		];

		solpts = Transpose[eigenvecs].Sqrt[DiagonalMatrix[eigenvals]];
		Return[solpts];
	];


End[]
EndPackage[]
