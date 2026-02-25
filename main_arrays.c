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
// unfortunately typedefs are then missing so we have to write struct Color c;
// instead of Color c;

// wishlist for arrays

#define LEN(x) (sizeof((x)) / sizeof((x)[0]))

const float gravity = 0.1, rebound_n = 1.1, rebound_nt = 0.90, rebound_p = 0.95,
            radius = 20, length0 = 300, springk = 1, speed_max = 10;

#define clamp(x, low, high) (x) < (low) ? low : ((x) > (high) ? high : (x))

int nframe = 35;
int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(144);
  while (!WindowShouldClose()) {
    BeginDrawing();
    ClearBackground(BLACK);

#define np 2
    static float x[np] = {200, 400}, y[np] = {400, 200};
    static float vx[np] = {0, 0}, vy[np] = {0, 0};
    static bool hover[np] = {false, false}, dragging[np] = {false, false};

#define nt 5000
    static float xtrail[np][nt];
    static float ytrail[np][nt];
    static float vytrail[np][nt];
    static int traili = 0;
    static int traili_max = 0;

    struct Vector2 p = GetMousePosition();

    float dx = x[0] - x[1], dy = y[0] - y[1], dl = hypotf(dx, dy),
          dlength = dl / length0 - 1;
    // color proportional to stretch?
    // droop with slack?
    // collision
    DrawLine(x[0], y[0], x[1], y[1], PURPLE);
    for (int i = 0; i < np; ++i) {

      xtrail[i][traili] = x[i];
      ytrail[i][traili] = y[i];
      vytrail[i][traili] = 0;
      if (traili > traili_max)
        traili_max = traili;

      for (int j = 1; j < traili_max; j++) {
        vytrail[i][j - 1] -= 1e-3;
        ytrail[i][j - 1] += vytrail[i][j - 1];
        DrawLine(xtrail[i][j % traili_max], ytrail[i][j % traili_max],
                 xtrail[i][j - 1], ytrail[i][j - 1],
                 ColorLerp(DARKPURPLE, BLACK,
                           (float)((traili - j + traili_max) % traili_max) /
                               traili_max));
      }

      float d = (p.x - x[i]) * (p.x - x[i]) + (p.y - y[i]) * (p.y - y[i]);
      hover[i] = d < radius * radius;

      if (hover[i] && IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
        dragging[i] = true;
      }
      if (IsMouseButtonDown(MOUSE_LEFT_BUTTON) && dragging[i]) {
        struct Vector2 d = GetMouseDelta();
        vx[i] = d.x;
        vy[i] = d.y;
      }
      if (IsMouseButtonReleased(MOUSE_LEFT_BUTTON)) {
        dragging[i] = false;
      }

      float speed = hypotf(vx[i], vy[i]);
      if (speed > speed_max) {
        vx[i] *= speed_max / speed;
        vy[i] *= speed_max / speed;
      }

      y[i] += vy[i];
      x[i] += vx[i];

      if (y[i] - radius < 0 || y[i] + radius > GetScreenHeight()) {
        vy[i] = -vy[i] * ((y[i] < radius) ? rebound_nt : rebound_n);
        vx[i] = vx[i] * rebound_p + 0.1 * vy[i];
        y[i] = clamp(y[i], radius, GetScreenHeight() - radius);
      }
      if (x[i] - radius < 0 || x[i] + radius > GetScreenWidth()) {
        vx[i] = -vx[i] * rebound_n;
        vy[i] = vy[i] * rebound_p;
        x[i] = clamp(x[i], radius, GetScreenWidth() - radius);
      }
      if (!dragging[i]) {
        vy[i] += gravity;
        // spring force
        vx[i] += (x[(i + 1) % 2] - x[i]) / dl * dlength * springk;
        vy[i] += (y[(i + 1) % 2] - y[i]) / dl * dlength * springk;
      }

      if (IsKeyPressed(KEY_P)) {
        x[i] = 200 + 20 * i;
        y[i] = 200 + 20 * i;
        vx[i] = 0;
        vy[i] = 0;
      }

      DrawCircle(x[i], y[i], radius, hover[i] ? PURPLE : DARKPURPLE);
    }
    if (++traili >= nt)
      traili = 0;

    EndDrawing();
  }
  CloseWindow();
}
