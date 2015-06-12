(* ::Package:: *)

BeginPackage["GPUDistances`", {"CUDALink`", "OpenCLLink`"}]

GPUDistances::nogpu = 
	"GPU computing is not supported"
GPUDistances::invinput = 
	"Inputs must be given as equal-sized 2D square matrices of Integer32 type"

CUDASSIM::usage = 
	"CUDASSIM[img1, img2] gives the structural similarity distance between two images, using CUDA"
OpenCLSSIM::usage = 
	"OpenCLSSIM[img1, img2] gives the structural similarity distance between two images, using OpenCL"


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

(* function to make sure inputs are well-formed *)
checkInputs[meminfoFn_, img1_, img2_]:=
	Module[{type1, type2, dims1, dims2}, 
		{{type1, dims1}, {type2, dims2}} = ({"Type", "Dimensions"} /. {meminfoFn[img1], meminfoFn[img2]});
		
		type1 == type2 == "Integer32" && 
		dims1 == dims2 && 
		Length[dims1] == 2 && 
		dims1[[1]] == dims1[[2]]
	];

(* distance function interfaces *)
CUDASSIM[img1_CUDAMemory, img2_CUDAMemory]:=
	gpuSSIM[CUDAQ, CUDAMemoryInformation, CUDAMemoryAllocate, CUDAMemoryGet, CUDAMemoryUnload,
			CUDASSIMkernel, img1, img2]

OpenCLSSIM[img1_OpenCLMemory, img2_OpenCLMemory]:=
	gpuSSIM[OpenCLQ, OpenCLMemoryInformation, OpenCLMemoryAllocate, OpenCLMemoryGet, OpenCLMemoryUnload,
			OpenCLSSIMkernel, img1, img2]

(* distance function implementations *)
gpuSSIM[gpuQ_, meminfoFn_, memalloc_, memget_, memfree_,
		kernel_, img1_, img2_]:=
	Module[{len, outmem, result}, 
		If[!gpuQ[],
			Message[GPUDistances::nogpu];
			Return[$Failed];
		];
		
		If[!checkInputs[meminfoFn, img1, img2],
			Message[GPUDistances::invinput];
			Return[$Failed];
		];
		
		len = First["Dimensions" /. meminfoFn[img1]];
		outmem = memalloc["Float", (len - 10)^2];
		
		kernel[img1, img2, outmem, len, {len - 10, len - 10}];
		result = Mean[memget[outmem]];
		
		memfree[outmem];
		
		Return[result];
	];


End[]
EndPackage[]
