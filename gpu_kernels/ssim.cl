#ifdef cl_khr_fp64
	#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#elif defined(cl_amd_fp64)
	#pragma OPENCL EXTENSION cl_amd_fp64 : enable
#else
	#error "This kernel requires double-precision support"
#endif

__constant double gaussian_kernel[11][11] = {{0.000003802917046317888,0.00001759448244809565,0.00006636107686176908,0.0001945573540794799,0.0004122408174475112,0.0005609936362550451,0.0004122408174475112,0.0001945573540794799,0.00006636107686176908,0.00001759448244809565,0.000003802917046317888},{0.00001759448244809565,0.0000814021996393737,0.0003070245256103131,0.000900134267933542,0.001907263224158832,0.002595479356074726,0.001907263224158832,0.000900134267933542,0.0003070245256103131,0.0000814021996393737,0.00001759448244809565},{0.00006636107686176908,0.0003070245256103131,0.001158003834587327,0.003395034751176518,0.007193621170012245,0.00978936468007629,0.007193621170012245,0.003395034751176518,0.001158003834587327,0.0003070245256103131,0.00006636107686176908},{0.0001945573540794799,0.000900134267933542,0.003395034751176518,0.00995356027107092,0.02109025301085816,0.02870045183627817,0.02109025301085816,0.00995356027107092,0.003395034751176518,0.000900134267933542,0.0001945573540794799},{0.0004122408174475112,0.001907263224158832,0.007193621170012245,0.02109025301085816,0.04468740430042675,0.06081239016679303,0.04468740430042675,0.02109025301085816,0.007193621170012245,0.001907263224158832,0.0004122408174475112},{0.0005609936362550451,0.002595479356074726,0.00978936468007629,0.02870045183627817,0.06081239016679303,0.0827559097623164,0.06081239016679303,0.02870045183627817,0.00978936468007629,0.002595479356074726,0.0005609936362550451},{0.0004122408174475112,0.001907263224158832,0.007193621170012245,0.02109025301085816,0.04468740430042675,0.06081239016679303,0.04468740430042675,0.02109025301085816,0.007193621170012245,0.001907263224158832,0.0004122408174475112},{0.0001945573540794799,0.000900134267933542,0.003395034751176518,0.00995356027107092,0.02109025301085816,0.02870045183627817,0.02109025301085816,0.00995356027107092,0.003395034751176518,0.000900134267933542,0.0001945573540794799},{0.00006636107686176908,0.0003070245256103131,0.001158003834587327,0.003395034751176518,0.007193621170012245,0.00978936468007629,0.007193621170012245,0.003395034751176518,0.001158003834587327,0.0003070245256103131,0.00006636107686176908},{0.00001759448244809565,0.0000814021996393737,0.0003070245256103131,0.000900134267933542,0.001907263224158832,0.002595479356074726,0.001907263224158832,0.000900134267933542,0.0003070245256103131,0.0000814021996393737,0.00001759448244809565},{0.000003802917046317888,0.00001759448244809565,0.00006636107686176908,0.0001945573540794799,0.0004122408174475112,0.0005609936362550451,0.0004122408174475112,0.0001945573540794799,0.00006636107686176908,0.00001759448244809565,0.000003802917046317888}};

//finds a ListCorrelate-element with gaussian_kernel and a single matrix
inline double correlate_one(__local int img[26][26], int xIndex, int yIndex){
	double result = 0;
	
	for (int i = 0; i < 11; ++i)
		for (int j = 0; j < 11; ++j)
			result += gaussian_kernel[i][j] * img[yIndex + i][xIndex + j];
	
	return result;
}

//finds a ListCorrelate-element with gaussian_kernel and the product of two matrices
inline double correlate_two(__local int img1[26][26], __local int img2[26][26], int xIndex, int yIndex){
	double result = 0;
	
	for (int i = 0; i < 11; ++i)
		for (int j = 0; j < 11; ++j)
			result += gaussian_kernel[i][j] * img1[yIndex + i][xIndex + j] * img2[yIndex + i][xIndex + j];
	
	return result;
}

//the SSIM kernel - N is the side length of the images
//call with block dimensions of 16*16
__kernel void ssim(__global const int *img1, __global const int *img2, __global double *out, int N){
	int xOffset = get_local_size(0)*get_group_id(0);
	int yOffset = get_local_size(1)*get_group_id(1);
	int xIndex = get_local_id(0);
	int yIndex = get_local_id(1);
	
	//step 1: cache the memory accesses
	
	//cache the elements used in the thread block - note: should use 16*16 blocks
	__local int img1_cache[26][26];
	__local int img2_cache[26][26];
	
	//most threads fetch 4 elements per image, in this pattern: x x
	//															x x
	if (xOffset + 2*xIndex + 1 < N && yOffset + 2*yIndex + 1 < N && //make sure we don't go out of bounds on the last block
		xIndex < 13 && yIndex < 13){ //we're fetching 26*26 elements, so only 26/2 = 13 threads should be active
		for (int i = 0; i < 2; ++i){
			for (int j = 0; j < 2; ++j){
				img1_cache[2*yIndex + i][2*xIndex + j] = img1[N*(yOffset + 2*yIndex + i) + (xOffset + 2*xIndex + j)];
				img2_cache[2*yIndex + i][2*xIndex + j] = img2[N*(yOffset + 2*yIndex + i) + (xOffset + 2*xIndex + j)];
			}
		}
	}
	
	barrier(CLK_LOCAL_MEM_FENCE); //wait to finish memory fetches
	
	//step 2: compute
	
	//we run a thread for each element of the input arrays - make sure we don't go out of bounds
	if (xOffset+xIndex > N-11 || yOffset+yIndex > N-11)
		return;
	
	//now do the computation
	double m1 = correlate_one(img1_cache, xIndex, yIndex);
	double m2 = correlate_one(img2_cache, xIndex, yIndex);
	
	double m1sq = m1*m1;
	double m2sq = m2*m2;
	double m1m2 = m1*m2;
	
	double sigma1sq = correlate_two(img1_cache, img1_cache, xIndex, yIndex) - m1sq;
	double sigma2sq = correlate_two(img2_cache, img2_cache, xIndex, yIndex) - m2sq;
	double sigma12 = correlate_two(img1_cache, img2_cache, xIndex, yIndex) - m1m2;
	
	double ssim = ((0.0001 + 2*m1m2)*(0.0009 + 2*sigma12)) / ((0.0001 + m1sq + m2sq)*(0.0009 + sigma1sq + sigma2sq));
	out[(N-10)*(yOffset + yIndex) + (xOffset + xIndex)] = ssim;
}
