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
CUDAEuclidean::usage = 
	"CUDASSIM[img1, img2] gives the Euclidean distance between two images, using CUDA"
CUDAManhattan::usage = 
	"CUDASSIM[img1, img2] gives the Manhattan distance between two images, using CUDA"

OpenCLSSIM::usage = 
	"OpenCLSSIM[img1, img2] gives the Structural Similarity distance between two images, using OpenCL"
OpenCLEuclidean::usage = 
	"OpenCLSSIM[img1, img2] gives the Euclidean distance between two images, using OpenCL"
OpenCLManhattan::usage = 
	"OpenCLSSIM[img1, img2] gives the Manhattan distance between two images, using OpenCL"


Begin["`Private`"]

(* load the kernels *)
If[CUDAQ[], 
	CUDASSIMkernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cu"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
	
	CUDAEuclideankernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "euclidean.cu"}]}, "euclidean", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
	
	CUDAManhattankernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "manhattan.cu"}]}, "manhattan", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
];

If[OpenCLQ[],
	OpenCLSSIMkernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cl"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
	
	OpenCLEuclideankernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "euclidean.cl"}]}, "euclidean", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
	
	OpenCLManhattankernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "manhattan.cl"}]}, "manhattan", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}, "CompileOptions"->{"-O3"}];
];

CUDASSIM[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{len, outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		If[First@OptionValue[CUDAMemoryInformation@img1, "Type"] != "Integer32" || First@OptionValue[CUDAMemoryInformation@img2, "Type"] != "Integer32",
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

CUDAEuclidean[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{len, outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		If[First@OptionValue[CUDAMemoryInformation@img1, "Type"] != "Integer32" || First@OptionValue[CUDAMemoryInformation@img2, "Type"] != "Integer32",
			Message[GPUDistances::invtype];
			Return[$Failed];
		];
		
		len = First@OptionValue[CUDAMemoryInformation@img1, "Dimensions"];
		outmem = CUDAMemoryAllocate["Integer32", len^2];
		
		CUDAEuclideankernel[img1, img2, outmem, len, {len, len}];
		result = Sqrt[Total[CUDAMemoryGet[outmem]]];
		
		CUDAMemoryUnload[outmem];
		
		Return[result];
	];

CUDAManhattan[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{len, outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		If[First@OptionValue[CUDAMemoryInformation@img1, "Type"] != "Integer32" || First@OptionValue[CUDAMemoryInformation@img2, "Type"] != "Integer32",
			Message[GPUDistances::invtype];
			Return[$Failed];
		];
		
		len = First@OptionValue[CUDAMemoryInformation@img1, "Dimensions"];
		outmem = CUDAMemoryAllocate["Integer32", len^2];
		
		CUDAManhattankernel[img1, img2, outmem, len, {len, len}];
		result = Total[CUDAMemoryGet[outmem]];
		
		CUDAMemoryUnload[outmem];
		
		Return[result];
	];

OpenCLSSIM[img1_, img2_]:=
	Module[{len, outmem, result}, 
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		If[First@OptionValue[OpenCLMemoryInformation@img1, "Type"] != "Integer32" || First@OptionValue[OpenCLMemoryInformation@img2, "Type"] != "Integer32",
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

OpenCLEuclidean[img1_OpenCLMemory, img2_OpenCLMemory]:=
	Module[{len, outmem, result},
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		If[First@OptionValue[OpenCLMemoryInformation@img1, "Type"] != "Integer32" || First@OptionValue[OpenCLMemoryInformation@img2, "Type"] != "Integer32",
			Message[GPUDistances::invtype];
			Return[$Failed];
		];
		
		len = First@OptionValue[OpenCLMemoryInformation@img1, "Dimensions"];
		outmem = OpenCLMemoryAllocate["Integer32", len^2];
		
		OpenCLEuclideankernel[img1, img2, outmem, len, {len, len}];
		result = Sqrt[Total[OpenCLMemoryGet[outmem]]];
		
		OpenCLMemoryUnload[outmem];
		
		Return[result];
	];

OpenCLManhattan[img1_OpenCLMemory, img2_OpenCLMemory]:=
	Module[{len, outmem, result},
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		If[First@OptionValue[OpenCLMemoryInformation@img1, "Type"] != "Integer32" || First@OptionValue[OpenCLMemoryInformation@img2, "Type"] != "Integer32",
			Message[GPUDistances::invtype];
			Return[$Failed];
		];
		
		len = First@OptionValue[OpenCLMemoryInformation@img1, "Dimensions"];
		outmem = OpenCLMemoryAllocate["Integer32", len^2];
		
		OpenCLManhattankernel[img1, img2, outmem, len, {len, len}];
		result = Total[OpenCLMemoryGet[outmem]];
		
		OpenCLMemoryUnload[outmem];
		
		Return[result];
	];


End[]
EndPackage[]
