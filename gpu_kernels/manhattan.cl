//the Euclidean distance kernel - N is the side length of the images
__global__ void euclidean(const int *vect1, const int *vect2, int *out, int N){
	int xIndex = get_global_id(0);
    int yIndex = get_global_id(1);
	
	if (xIndex >= N || yIndex >= N)
		return;
	
	int index = N*yIndex + xIndex;
	out[index] = abs(vect1[index] - vect2[index]);
}
