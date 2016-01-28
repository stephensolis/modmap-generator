(* ::Package:: *)

BeginPackage["CGR`"]

FCGR::usage = 
	"FCGR[seq, alphabet, k] gives the grayscale Chaos Game Representation plot of a DNA sequence in a 2D square of resolution 2^k."

SparseFCGR::usage = 
	"SparseFCGR[seq, alphabet, k] gives the same result as FCGR[], but as a sparse matrix."


Begin["`Private`"]

FCGR[seq_String, cgrOrder_List, k_Integer?Positive]:=
	Module[{chars},
		chars = ToCharacterCode[StringReplace[seq, Except[cgrOrder]->""]];
		
		Return[FCGRHelper[chars, Flatten@ToCharacterCode[cgrOrder], k]];
	];

FCGRHelper = Compile[{{chars, _Integer, 1}, {cgrOrder, _Integer, 1}, {k, _Integer}},
	Module[{resimg, x, y, char, i},
		resimg = ConstantArray[0, {2^k, 2^k}];
		
		x = 2^(k-1);
		y = 2^(k-1);
		Do[
			char = chars[[i]];
			
			x = Quotient[x, 2];
			If[char == cgrOrder[[3]] || char == cgrOrder[[4]], 
				x += 2^(k-1);
			];
			
			y = Quotient[y, 2];
			If[char == cgrOrder[[1]] || char == cgrOrder[[4]], 
				y += 2^(k-1);
			];
			
			If[i >= k,
				resimg[[y+1, x+1]]++;
			];
		, {i, Length@chars}];
		
		resimg
	]
, RuntimeOptions -> "Speed"];

SparseFCGR[seq_String, {a1_, a2_, a3_, a4_}, k_Integer?Positive]:=
	Module[{shifts, pts, arrayrules, resimg},
		shifts = Characters[seq] /. {a1->{0, 0}, a2->{0, 2^k}, a3->{2^k, 2^k}, a4->{2^k, 0}, _?StringQ->Sequence[]};
		
		pts = FoldList[IntegerPart[(#+#2)/2]&, 2^(k-1), #]& /@ Transpose[shifts];
		pts = Drop[pts, None, k];
		
		arrayrules = Rule @@@ Tally[Transpose[{2^k-pts[[2]], 1+pts[[1]]}]];
		resimg = SparseArray[arrayrules, {2^k, 2^k}, 0];
		
		Return[resimg];
	];


End[]
EndPackage[]
