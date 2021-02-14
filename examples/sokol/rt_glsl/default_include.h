#define HANDMADE_MATH_IMPLEMENTATION
#define HANDMADE_MATH_NO_SSE
#include "HandmadeMath.h"

void calc_matrices(void *res, float w, float h, float rx, float ry, float scale){
	hmm_mat4 proj = HMM_Perspective(60.0f, w/h, 0.01f, 10.0f);
	hmm_mat4 view = HMM_LookAt(HMM_Vec3(0.0f, 0.0f, 6.0f), HMM_Vec3(0.0f, 0.0f, 0.0f), HMM_Vec3(0.0f, 1.0f, 0.0f));
	hmm_mat4 view_proj = HMM_MultiplyMat4(proj, view);
	//state.rx += 1.0f; state.ry += 2.0f;

	
	hmm_mat4 rxm = HMM_Rotate(rx, HMM_Vec3(1.0f, 0.0f, 0.0f));
	hmm_mat4 rym = HMM_Rotate(ry, HMM_Vec3(0.0f, 1.0f, 0.0f));

	hmm_mat4 model = HMM_MultiplyMat4(rxm, rym);
	hmm_mat4 scale_mx = HMM_Scale(HMM_Vec3(scale, scale, scale));
	model = HMM_MultiplyMat4(model, scale_mx);
	hmm_mat4 tmp_res = HMM_MultiplyMat4(view_proj, model);

	// copy the matrix to V
	int i = 0;
	float *p = &tmp_res.Elements[0];
	while(i < 16){
		((float*)(res))[i]= p[i];
		i++;
	}
}