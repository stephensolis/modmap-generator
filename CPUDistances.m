(* ::Package:: *)

BeginPackage["CPUDistances`"]

Descriptor::usage = 
	"Descriptor[img, winsizes, steps, histbins] gives the descriptor of a matrix, given the windowsizes, windowsteps and histogram bins"
Descriptor::lengtherr = 
	"winsizes and steps must have the same length"

SSIM::usage = 
	"SSIM[img1, img2] gives the Structural Similarity distance between two images"

ApproxInfoDist::usage = 
	"ApproxInfoDist[img1, img2] gives the Approximate Information distance between two images"


Begin["`Private`"]

Descriptor[img_List?MatrixQ, winsizes_List, steps_List, histbins_List]:=
	Module[{allwinds, currsize, currstep, winds},
		If[Length[winsizes] != Length[steps],
			Message[Descriptor::lengtherr];
			Return[$Failed];
		];
		
		allwinds = Map[(
			{currsize, currstep} = #;
			
			winds = Partition[img, {currsize, currsize}, currstep];
			
			(* note: Length/@BinLists is ~2x faster than BinCounts here *)
			Flatten@Map[(Length/@BinLists[Flatten[#], {histbins}]) / (currsize^2)&, winds, {2}]
		)&, Transpose[{winsizes, steps}]];
		
		Return[Flatten[allwinds]];
	];

SSIM = Compile[{{img1, _Integer, 2}, {img2, _Integer, 2}},
	Module[{w, c1, c2, m1, m2, m1sq, m2sq, m1m2, sigma1sq, sigma2sq, sigma12, ssimmap},
		(* like w = N[GaussianMatrix[{5, 1.5}], 6] - this is 15-20% faster *)
		w = {{0.00000380292, 0.0000175945, 0.0000663611, 0.000194557, 0.000412241, 0.000560994, 0.000412241, 0.000194557, 0.0000663611, 0.0000175945, 0.00000380292}, 
			 {0.0000175945, 0.0000814022, 0.000307025, 0.000900134, 0.00190726, 0.00259548, 0.00190726, 0.000900134, 0.000307025, 0.0000814022, 0.0000175945}, 
			 {0.0000663611, 0.000307025, 0.001158, 0.00339503, 0.00719362, 0.00978936, 0.00719362, 0.00339503, 0.001158, 0.000307025, 0.0000663611}, 
			 {0.000194557, 0.000900134, 0.00339503, 0.00995356, 0.0210903, 0.0287005, 0.0210903, 0.00995356, 0.00339503, 0.000900134, 0.000194557}, 
			 {0.000412241, 0.00190726, 0.00719362, 0.0210903, 0.0446874, 0.0608124, 0.0446874, 0.0210903, 0.00719362, 0.00190726, 0.000412241}, 
			 {0.000560994, 0.00259548, 0.00978936, 0.0287005, 0.0608124, 0.0827559, 0.0608124, 0.0287005, 0.00978936, 0.00259548, 0.000560994}, 
			 {0.000412241, 0.00190726, 0.00719362, 0.0210903, 0.0446874, 0.0608124, 0.0446874, 0.0210903, 0.00719362, 0.00190726, 0.000412241}, 
			 {0.000194557, 0.000900134, 0.00339503, 0.00995356, 0.0210903, 0.0287005, 0.0210903, 0.00995356, 0.00339503, 0.000900134, 0.000194557}, 
			 {0.0000663611, 0.000307025, 0.001158, 0.00339503, 0.00719362, 0.00978936, 0.00719362, 0.00339503, 0.001158, 0.000307025, 0.0000663611}, 
			 {0.0000175945, 0.0000814022, 0.000307025, 0.000900134, 0.00190726, 0.00259548, 0.00190726, 0.000900134, 0.000307025, 0.0000814022, 0.0000175945}, 
			 {0.00000380292, 0.0000175945, 0.0000663611, 0.000194557, 0.000412241, 0.000560994, 0.000412241, 0.000194557, 0.0000663611, 0.0000175945, 0.00000380292}};
		c1 = 0.01^2;
		c2 = 0.03^2;
		
		m1 = ListCorrelate[w, img1];
		m2 = ListCorrelate[w, img2];
		
		m1sq = m1^2;
		m2sq = m2^2;
		m1m2 = m1*m2;
		
		sigma1sq = ListCorrelate[w, img1^2] - m1sq;
		sigma2sq = ListCorrelate[w, img2^2] - m2sq;
		sigma12 = ListCorrelate[w, img1*img2] - m1m2;
		
		ssimmap = ((c1 + 2*m1m2)*(c2 + 2*sigma12)) / ((c1 + m1sq + m2sq)*(c2 + sigma1sq + sigma2sq));
		
		Mean[Mean[ssimmap]]
	]
, RuntimeOptions -> "Speed"];

ApproxInfoDist[img1_List?MatrixQ, img2_List?MatrixQ]:=
	Module[{x, y, xy},
		x = Total[Unitize[img1], 2]; 
		y = Total[Unitize[img2], 2];
		xy = Total[Unitize[img1 + img2], 2];

		Return[(2 xy - x - y)/xy];
	];


End[]
EndPackage[]
