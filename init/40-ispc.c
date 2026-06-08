#ifndef RAYLIBD
#include "40-f.h"
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
#include <stdbool.h>

#define ni 84
const Color palette[] = {GREEN, YELLOW, RED};

const int n3 = ni * ni * ni;
const float den = 20.0f;

signed char vox[ni * ni * ni]; //  = MemAlloc((size_t)n3 * sizeof(*vox));

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "40-ispc.c");
  SetTargetFPS(10);

  float vin[16], vout[16];
  for (int i = 0; i < 16; ++i)
    vin[i] = i;

  while (!WindowShouldClose()) {
    fill_scene_dense(ni, den, vox, n3);

    BeginDrawing();
    ClearBackground(BLACK);

    float th = GetTime() / 2.0f;
    BeginMode3D((Camera3D){10 * sinf(th), 10 * cosf(th), 2, 0, 0,
                           sinf(5 * th) / 2, 0, 0, 10, 7, CAMERA_ORTHOGRAPHIC});

    const float dx = (float)ni / 2.0f;
    for (int idx = 0; idx < n3; ++idx) {
      signed char ci = vox[idx];
      if (ci < 0)
        continue;

      int i = idx / (ni * ni);
      int rem = idx % (ni * ni);
      int j = rem / ni;
      int k = rem % ni;

      Vector3 p = (Vector3){(i - dx) / den, (j - dx) / den, (k - dx) / den};
      DrawPoint3D(p, ColorAlpha(palette[(int)ci], 0.22f));
    }

    EndMode3D();
    EndDrawing();
  }

  MemFree(vox);
  CloseWindow();
  return 0;
}
