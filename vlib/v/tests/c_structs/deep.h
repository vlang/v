#ifndef __DEEP_H__
#define __DEEP_H__

struct DeepStruct{
	int A1;
	struct {
		int A2;
		struct {
		       int A3;
		       struct {
				int A4;
		       } S3;
                } S2;
        } S1;
};
#endif
