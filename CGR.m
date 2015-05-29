(* ::Package:: *)

BeginPackage["CGR`"]

FCGR::usage = 
	"FCGR[seq, alphabet, k] gives the grayscale Chaos Game Representation plot of a DNA sequence in a 2D square of resolution 2^k."

SparseFCGR::usage = 
	"SparseFCGR[seq, alphabet, k] gives the same result as FCGR[], but as a sparse matrix."


Begin["`Private`"]

SparseFCGR[seq_String, {a1_, a2_, a3_, a4_}, k_Integer?Positive]:=
	Module[{shifts, pts, arrayrules, resimg},
		shifts = Characters[seq] /. {a1->{0, 0}, a2->{0, 2^k}, a3->{2^k, 2^k}, a4->{2^k, 0}, _?StringQ->Sequence[]};
		
		pts = FoldList[IntegerPart[(#+#2)/2]&, 2^(k-1), #]& /@ Transpose[shifts];
		pts = Drop[#, k]& /@ pts;
		
		arrayrules = Rule @@@ Tally[Transpose[{2^k-pts[[2]], 1+pts[[1]]}]];
		resimg = SparseArray[arrayrules, {2^k, 2^k}, 0];
		
		Return[resimg];
	];

FCGR = Normal @* SparseFCGR;


End[]
EndPackage[]
