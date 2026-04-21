#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
#include <stdbool.h>
// exclude includes for language-c's pass can't parse raylib.h

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(144);
  while (!WindowShouldClose()) {
    BeginDrawing();
    ClearBackground(BLACK);
    DrawText("Hello Raylibd", 10, 10, 20, DARKGREEN);
    EndDrawing();
  }
  CloseWindow();
}
