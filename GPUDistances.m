(* ::Package:: *)

BeginPackage["GPUDistances`", {"CUDALink`", "OpenCLLink`"}]

GPUDistances::nocuda = 
	"CUDA is not supported"
GPUDistances::noopencl = 
	"OpenCL is not supported"

CUDASSIM::usage = 
	"CUDASSIM[img1, img2] gives the Structural Similarity distance between two images, using CUDA"
OpenCLSSIM::usage = 
	"OpenCLSSIM[img1, img2] gives the Structural Similarity distance between two images, using OpenCL"


Begin["`Private`"]

(* load the kernels *)
If[CUDAQ[], 
	CUDASSIMkernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cu"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}];
];

If[OpenCLQ[],
	OpenCLSSIMkernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cl"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}];
];

CUDASSIM[img1_, img2_]:=
	Module[{ssimmap},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		ssimmap = CUDAMemoryAllocate["Float", (Length@img1 - 10)^2];
		
		CUDASSIMkernel[img1, img2, ssimmap, Length@img1];
		
		Return[Mean[CUDAMemoryGet[ssimmap]]];
	];

OpenCLSSIM[img1_, img2_]:=
	Module[{ssimmap}, 
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		ssimmap = OpenCLMemoryAllocate["Float", (Length@img1 - 10)^2];
		
		OpenCLSSIMkernel[img1, img2, ssimmap, Length@img1];
		
		Return[Mean[OpenCLMemoryGet[ssimmap]]];
	];


End[]
EndPackage[]
