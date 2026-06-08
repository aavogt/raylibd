#ifndef RAYLIBD
#include "41-f.h"
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif
#include <stdbool.h>

#define ni 150

const int n3 = ni * ni * ni;
const float den = 32.0f;

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "41-ispc-texture.c");
  SetTargetFPS(60);

  while (!WindowShouldClose()) {

    signed char vox[ni * ni * ni];
    fill_scene_dense(ni, den, vox, n3);

    BeginDrawing();
    ClearBackground(BLACK);

    float th = GetTime() / 2.0f;
    // Render the voxel field into a 2D RGBA texture using ISPC (orthographic
    // projection). This avoids DrawPoint3D per-voxel like init/40-ispc.c,
    // allowing higher FPS, but it looks like there's still extra copying to
    // avoid.

    const float dx = (float)ni / 2.0f;

    int screenW = GetScreenWidth();
    int screenH = GetScreenHeight();

    static Texture2D tex = {0};
    static unsigned char *pixels = NULL;
    static int tex_w = 0, tex_h = 0;

    if (tex_w != screenW || tex_h != screenH || tex.id == 0) {
      if (pixels)
        free(pixels);
      tex_w = screenW;
      tex_h = screenH;
      pixels = malloc(tex_w * tex_h * 4);
      memset(pixels, 0, tex_w * tex_h * 4);

      Image img = GenImageColor(tex_w, tex_h, BLANK);
      Texture2D newt = LoadTextureFromImage(img);
      UnloadImage(img);
      if (tex.id != 0)
        UnloadTexture(tex);
      tex = newt;
    } else {
      // clear
      memset(pixels, 0, tex_w * tex_h * 4);
    }

    // Camera parameters (match previous behaviour)
    Vector3 camPos = (Vector3){10 * sinf(th), 10 * cosf(th), 2};
    Vector3 camTarget = (Vector3){0, 0, sinf(5 * th) / 2};
    Vector3 camUp = (Vector3){0, 0, 10};

    Vector3 forward = Vector3Normalize(Vector3Subtract(camTarget, camPos));
    Vector3 right = Vector3Normalize(Vector3CrossProduct(forward, camUp));
    Vector3 up = Vector3CrossProduct(right, forward);

    // ortho "size" roughly matches previous fovy parameter used in BeginMode3D
    float ortho_size = 4.0f;

    // Call ISPC to rasterize voxels into the pixel buffer (no depth test, last
    // write wins)
    fill_texture_ortho(ni, den, screenW, screenH, camPos.x, camPos.y, camPos.z,
                       right.x, right.y, right.z, up.x, up.y, up.z, ortho_size,
                       pixels, tex_w * tex_h * 4);

    // Push pixel data into the GPU texture and draw it as a 2D quad
    UpdateTexture(tex, pixels);
    DrawTexture(tex, 0, 0, WHITE);
    EndDrawing();
  }

  CloseWindow();
  return 0;
}
