(* ::Package:: *)

BeginPackage["GPUDistances`", {"CUDALink`", "OpenCLLink`"}]

GPUDistances::nocuda = 
	"CUDA is not supported"
GPUDistances::noopencl = 
	"OpenCL is not supported"

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
						{16, 16}];
	
	CUDAEuclideankernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "euclidean.cu"}]}, "euclidean", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}];
	
	CUDAManhattankernel = CUDAFunctionLoad[{FileNameJoin[{"gpu_kernels", "manhattan.cu"}]}, "manhattan", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}];
];

If[OpenCLQ[],
	OpenCLSSIMkernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "ssim.cl"}]}, "ssim", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Float", 1, "Output"}, "Integer32"}, 
						{16, 16}];
	
	OpenCLEuclideankernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "euclidean.cl"}]}, "euclidean", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}];
	
	OpenCLManhattankernel = OpenCLFunctionLoad[{FileNameJoin[{"gpu_kernels", "manhattan.cl"}]}, "manhattan", 
						{{"Integer32", 2, "Input"}, {"Integer32", 2, "Input"}, {"Integer32", 1, "Output"}, "Integer32"}, 
						{16, 16}];
];

CUDASSIM[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		outmem = CUDAMemoryAllocate["Float", (Length@img1 - 10)^2];
		
		CUDASSIMkernel[img1, img2, outmem, Length@img1];
		result = CUDATotal[outmem] / ((Length@img1 - 10)^2);
		
		CUDAMemoryUnload[outmem];
		
		Return[result];
	];

CUDAEuclidean[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		outmem = CUDAMemoryAllocate["Integer32", (Length@img1)^2];
		
		CUDAEuclideankernel[img1, img2, outmem, Length@img1];
		result = Sqrt[CUDATotal[outmem]];
		
		CUDAMemoryUnload[outmem];
		
		Return[result];
	];

CUDAManhattan[img1_CUDAMemory, img2_CUDAMemory]:=
	Module[{outmem, result},
		If[!CUDAQ[],
			Message[GPUDistances::nocuda];
			Return[$Failed];
		];
		
		outmem = CUDAMemoryAllocate["Integer32", (Length@img1)^2];
		
		CUDAManhattankernel[img1, img2, outmem, Length@img1];
		result = CUDATotal[outmem];
		
		CUDAMemoryUnload[outmem];
		
		Return[result];
	];

OpenCLSSIM[img1_OpenCLMemory, img2_OpenCLMemory]:=
	Module[{outmem, result}, 
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		outmem = OpenCLMemoryAllocate["Float", (Length@img1 - 10)^2];
		
		OpenCLSSIMkernel[img1, img2, outmem, Length@img1];
		result = Mean[OpenCLMemoryGet[outmem]];
		
		OpenCLMemoryUnload[outmem];
		
		Return[result];
	];

OpenCLEuclidean[img1_OpenCLMemory, img2_OpenCLMemory]:=
	Module[{outmem, result},
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		outmem = OpenCLMemoryAllocate["Integer32", (Length@img1)^2];
		
		OpenCLEuclideankernel[img1, img2, outmem, Length@img1];
		result = Sqrt[Total[OpenCLMemoryGet[outmem]]];
		
		OpenCLMemoryUnload[outmem];
		
		Return[result];
	];

OpenCLManhattan[img1_OpenCLMemory, img2_OpenCLMemory]:=
	Module[{outmem, result},
		If[!OpenCLQ[],
			Message[GPUDistances::noopencl];
			Return[$Failed];
		];
		
		outmem = OpenCLMemoryAllocate["Integer32", (Length@img1)^2];
		
		OpenCLManhattankernel[img1, img2, outmem, Length@img1];
		result = Total[OpenCLMemoryGet[outmem]];
		
		OpenCLMemoryUnload[outmem];
		
		Return[result];
	];


End[]
EndPackage[]
