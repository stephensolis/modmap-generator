(* ::Package:: *)

BeginPackage["GPUDistances`", {"CUDALink`", "OpenCLLink`"}]

GPUDistances::nocuda = 
	"CUDA is not supported"
GPUDistances::noopencl = 
	"OpenCL is not supported"
GPUDistances::invtype = 
	"Images must be given as matrices of Integer32 type"

CUDASSIM::usage = 
	"CUDASSIM[img1, img2] gives the Structural Similarity distance between two images, using CUDA"

OpenCLSSIM::usage = 
	"OpenCLSSIM[img1, img2] gives the Structural Similarity distance between two images, using OpenCL"


Begin["`Private`"]

(* load the kernels *)
If[CUDAQ[], 
	CUDASSIMkernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cu"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
];

If[OpenCLQ[],
	OpenCLSSIMkernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cl"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
];

CUDASSIM[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{len, outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		If[OptionValue[CUDAMemoryInformation@img1, "Type"] != "Integer32" || OptionValue[CUDAMemoryInformation@img2, "Type"] != "Integer32",
			Message[GPUDistances::invtype];
			Return[$Failed];
		];
		
		len = First@OptionValue[CUDAMemoryInformation@img1, "Dimensions"];
		outmem = CUDAMemoryAllocate["Float", (len - 10)^2];
		
		CUDASSIMkernel[img1, img2, outmem, len, {len - 10, len - 10}];
		result = Mean[CUDAMemoryGet[outmem]];
		
		CUDAMemoryUnload[outmem];
		
		Return[result];
	];

OpenCLSSIM[img1_, img2_]:=
	Module[{len, outmem, result}, 
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		If[OptionValue[OpenCLMemoryInformation@img1, "Type"] != "Integer32" || OptionValue[OpenCLMemoryInformation@img2, "Type"] != "Integer32",
			Message[GPUDistances::invtype];
			Return[$Failed];
		];
		
		len = First@OptionValue[OpenCLMemoryInformation@img1, "Dimensions"];
		outmem = OpenCLMemoryAllocate["Float", (len - 10)^2];
		
		OpenCLSSIMkernel[img1, img2, outmem, len, {len - 10, len - 10}];
		result = Mean[OpenCLMemoryGet[outmem]];
		
		OpenCLMemoryUnload[outmem];
		
		Return[result];
	];


End[]
EndPackage[]
