#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
#include <stdbool.h>
// 10-tint.fs makes the RAYWHITE circle come out pink
// change the constants and see the result change

#define WH 256
int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT);
  InitWindow(WH, WH, "raylibd minimal fragment shader");
  SetTargetFPS(10);
  Shader sh = LoadShader(0, "10-tint.fs");
  while (!WindowShouldClose()) {
    BeginDrawing();
    ClearBackground(DARKGRAY);
    BeginShaderMode(sh);
    DrawCircle(30, 100, 100, RAYWHITE);
    EndShaderMode();
    EndDrawing();
  }
  CloseWindow();
}
