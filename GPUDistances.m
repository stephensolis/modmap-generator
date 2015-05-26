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
	Module[{outmem},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		outmem = CUDAMemoryAllocate["Float", (Length@img1 - 10)^2];
		
		CUDASSIMkernel[img1, img2, outmem, Length@img1];
		
		Return[Mean[CUDAMemoryGet[outmem]]];
	];

OpenCLSSIM[img1_, img2_]:=
	Module[{outmem}, 
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		outmem = OpenCLMemoryAllocate["Float", (Length@img1 - 10)^2];
		
		OpenCLSSIMkernel[img1, img2, outmem, Length@img1];
		
		Return[Mean[OpenCLMemoryGet[outmem]]];
	];


End[]
EndPackage[]
