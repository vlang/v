typedef struct Camera3D {
    int a;
} Camera3D;

typedef  Camera3D Camera;

typedef struct Vector3 {
    float x;                // Vector x component
    float y;                // Vector y component
    float z;                // Vector z component
} Vector3;

void UpdateCamera(Camera *camera, int mode)
{
}

void UpdateCameraPro(Camera *Camera, Vector3 movement , Vector3 rotation, float zoom)
{
}