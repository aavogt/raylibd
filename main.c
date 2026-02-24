#ifndef RAYLIBD
#include "raylib.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
// exclude includes for language-c's pass can't parse raylib.h
// unfortunately typedefs are then missing so we have to write struct Color c;
// instead of Color c;

#define LEN(x) (sizeof((x)) / sizeof((x)[0]))

int nframe = 35;
int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(60);
  while (!WindowShouldClose()) {
    BeginDrawing();
    ClearBackground(BLACK);
    char text[] = "     rayskeleton!";
    static int n = 0, m = 0, x = 0;
    snprintf(text, 5, "%04d", n);
    struct Color cols[] = {PURPLE};
    if (n++ % nframe == 0) {
      if (++m >= LEN(cols))
        m = 0;
    }
    x += 1;
    int w = MeasureText(text, 20);
    for (int k = 0; k < 43; k++) {
      int w = 7;
      float fx = (x + 20 * k) % (GetScreenWidth() + w) - w;
      float y = 300 * (sin(PI * fx / GetScreenWidth())) - 30;
      DrawCircle(fx, y, w, cols[m]);
    }
    DrawText(text, GetScreenWidth() / 2 - w / 2, GetScreenHeight() / 2, 20,
             WHITE);
    EndDrawing();
  }
  CloseWindow();
}
