#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#endif
// https://claude.ai/chat/606e9587-0a41-4fd4-8575-5e33544309c6

const int pg = 4;
typedef struct {
  int8_t cx, cy;   // kernel center, fixed-point subpixel
  int32_t bw;      // index into precomputed falloff table
  uint8_t r, g, b; // color weight (signed for difference kernels)
  // time animation:
  uint8_t param;  // which kernel param varies
  int8_t amp;     // variation amplitude
  uint32_t omega; // frequency (for sin) or duration (for lerp)
} Kernel;         // ~10 bytes

#define N_KERNELS 1

const Kernel kernel0 = {10, 10, 10, 255, 200, 20, 0, 1, 2000};

Kernel kernels[N_KERNELS] = {kernel0};

#define min(a, b) a < b ? a : b

uint8_t exp_lut[512];
int8_t sin_lut[512];

void lut_init() {
  for (int i = 0; i < 255; i++) {
    exp_lut[i] = 255 * exp(-sqrt(i) / 8.0);
  }
  for (int i = 0; i < 512; i++)
    sin_lut[i] = 255 * sin(i / 512.0 * 3.14159);
}

Color pixels[256];
void ws2812_write(int r, int g, int b) {
  static int pixeli = 0;
  int rgba = ((r << 24) | (g << 16) | (b << 8) | 255);
  pixels[pixeli++] = *(Color *)(&rgba);
  pixeli = pixeli % 256;
}

void draw_pixels() {
  float h = GetScreenHeight();
  float w = GetScreenWidth();
  if (h > w)
    h = w;
  if (w > h)
    w = h;
  int pw = w / 16 - pg;
  int ph = h / 16 - pg;
  for (int i = 0; i < 256; i++) {
    DrawRectangle(pg + ((pw + pg) * (i / 16)), pg + (pw + pg) * (i % 16), pw,
                  ph, pixels[i]);
  }
}

int clamp(int a) { return (a > 255 ? 255 : (a < 0 ? 0 : a)); }

void render_frame(uint32_t t) {
  for (int x = 0; x < 16; x++) {
    for (int y = 0; y < 16; y++) {
      int32_t R = 0, G = 0, B = 0;
      for (int k = 0; k < N_KERNELS; k++) {
        int dy = (10 * (x - kernels[k].cx) + sin_lut[t]) / 10,
            dx = y - kernels[k].cy;
        if (dx < 0)
          dx = -dx;
        if (dy < 0)
          dy = -dy;
        int32_t zstat = (dx * dx + dy * dy) * kernels[k].bw;
        uint8_t w = exp_lut[min(zstat, 511)];
        R += (kernels[k].r * w) >> 8; // signed: negative = subtract
        G += (kernels[k].g * w) >> 8;
        B += (kernels[k].b * w) >> 8;
      }
      ws2812_write(clamp(R), clamp(G), clamp(B));
    }
  }
}

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(144);
  while (!WindowShouldClose()) {
    static float time = 0;
    static bool up = true;
    if (time * 10 > 512) {
      up = false;
      time = 51;
    }
    if (time <= 0) {
      up = true;
      time = 0;
    }
    if (up)
      time += 2 * GetFrameTime();
    else
      time -= 2 * GetFrameTime();
    lut_init();
    render_frame(time * 2);
    BeginDrawing();
    ClearBackground(BLACK);
    draw_pixels();
    EndDrawing();
  }
  CloseWindow();
}
