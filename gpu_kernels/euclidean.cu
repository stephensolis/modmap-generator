//the Euclidean distance kernel - N is the side length of the images
__global__ void euclidean(const int *vect1, const int *vect2, int *out, int N){
	int xIndex = blockDim.x*blockIdx.x + threadIdx.x;
    int yIndex = blockDim.y*blockIdx.y + threadIdx.y;
	
	if (xIndex >= N || yIndex >= N)
		return;
	
	int index = N*yIndex + xIndex;
	out[index] = (vect1[index] - vect2[index]) * (vect1[index] - vect2[index]);
}
