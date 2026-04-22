#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#endif

void shset(Shader sh, const char *uniformName, const void *res,
           int uniformType) {
  int loc = GetShaderLocation(sh, uniformName);
  SetShaderValue(sh, loc, res, uniformType);
}

Texture2D fillRedRandom(bool all_rows) {
  int w = GetScreenWidth();
  int h = GetScreenHeight();
  int n = all_rows ? w * h : w;
  Image noise = GenImageColor(w, h, BLACK);
  for (int i = 0; i < n; i++)
    ((Color *)noise.data)[i] =
        (Color){(unsigned char)(rand() % 256), 0, 0, 255};
  Texture2D result = LoadTextureFromImage(noise);
  UnloadImage(noise);
  return result;
}

static const int BRUSH_NOISE_FRAMES = 1000;

Texture2D brushNoise;
Image brushNoiseImage;
Texture2D texA;
RenderTexture2D rtA, rtB;
Shader sh, shg;
Shader brushSh;

void updateBrushNoise() {
  int w = GetScreenWidth();
  int h = GetScreenHeight();
  if (brushNoise.id)
    UnloadTexture(brushNoise);
  if (brushNoiseImage.data)
    UnloadImage(brushNoiseImage);
  brushNoiseImage = GenImageColor(w, h, BLACK);
  for (int i = 0; i < w * h; i++)
    ((Color *)brushNoiseImage.data)[i] =
        (Color){(unsigned char)(rand() % 256), 0, 0, 255};
  brushNoise = LoadTextureFromImage(brushNoiseImage);
}

void brushTool(Vector2 mouse, bool useNoise, float radius, Color solidColor) {
  if (!brushNoise.id)
    updateBrushNoise();

  Vector4 norm = ColorNormalize(solidColor);
  float colorVec[4] = {norm.x, norm.y, norm.z, 0.0};
  float mouseScaleX = (float)rtA.texture.width / (float)GetScreenWidth();
  float mouseScaleY = (float)rtA.texture.height / (float)GetScreenHeight();
  float mouseVec[2] = {
      mouse.x,
      mouse.y,
  };
  int useNoiseInt = useNoise ? 1 : 0;

  shset(brushSh, "mouse", mouseVec, SHADER_UNIFORM_VEC2);
  shset(brushSh, "radius", &radius, SHADER_UNIFORM_FLOAT);
  shset(brushSh, "useNoise", &useNoiseInt, SHADER_UNIFORM_INT);
  shset(brushSh, "solidColor", colorVec, SHADER_UNIFORM_VEC4);
  SetShaderValueTexture(brushSh, GetShaderLocation(brushSh, "noiseTex"),
                        brushNoise);

  BeginTextureMode(rtA);
  BeginShaderMode(brushSh);
  DrawTexture(rtB.texture, 0, 0, WHITE);
  EndShaderMode();
  EndTextureMode();
  BeginTextureMode(rtB);
  BeginShaderMode(brushSh);
  DrawTexture(rtA.texture, 0, 0, WHITE);
  EndShaderMode();
  EndTextureMode();
}

/// one step of the turing pattern
void pingpongRT() {
  BeginTextureMode(rtB);
  BeginShaderMode(sh);
  DrawTexture(rtA.texture, 0, 0, WHITE);
  EndShaderMode();
  EndTextureMode();

  BeginTextureMode(rtA);
  BeginShaderMode(sh);
  DrawTexture(rtB.texture, 0, 0, WHITE);
  EndShaderMode();
  EndTextureMode();
}

void allocRT() {
  int w = GetScreenWidth(), h = GetScreenHeight();
  rtA = LoadRenderTexture(w, h);
  rtB = LoadRenderTexture(w, h);
}

void fillRT() {
  UnloadTexture(texA);
  texA = fillRedRandom(false);
  BeginTextureMode(rtA);
  DrawTexture(texA, 0, 0, WHITE);
  EndTextureMode();
  BeginTextureMode(rtB);
  DrawTexture(texA, 0, 0, WHITE);
  EndTextureMode();
}

void reallocRT() {
  int w = GetScreenWidth(), h = GetScreenHeight();
  UnloadRenderTexture(rtA);
  UnloadRenderTexture(rtB);
  rtA = LoadRenderTexture(w, h);
  rtB = LoadRenderTexture(w, h);
  fillRT();
  updateBrushNoise();
  float res[2] = {w, h};
  shset(sh, "resolution", res, SHADER_UNIFORM_VEC2);
  shset(brushSh, "resolution", res, SHADER_UNIFORM_VEC2);
}

FILE *ffmpeg_pipe;

int vidw, vidh;

int main(void) {

  SetConfigFlags( // FLAG_WINDOW_RESIZABLE |
      FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT);
  InitWindow(540, 1200, "Turing Pattern");
  srand(time(NULL));

  sh = LoadShader(0, "11-step.fs");
  brushSh = LoadShader(0, "11-brush.fs");

  shset(sh, "resolution", (float[]){GetScreenWidth(), GetScreenWidth()},
        SHADER_UNIFORM_VEC2);
  shset(brushSh, "resolution",
        (float[]){(float)GetScreenWidth(), (float)GetScreenHeight()},
        SHADER_UNIFORM_VEC2);

  allocRT();
  fillRT();
  updateBrushNoise();

  int frameCount = 0;
  SetTargetFPS(144);
  while (!WindowShouldClose()) {
    if (IsWindowResized() && (rtA.texture.height < GetScreenHeight() ||
                              rtA.texture.width < GetScreenWidth()))
      reallocRT();

    if (IsKeyPressed(KEY_ENTER)) {
      char cmd[200];
      printf("recording\n");
      vidw = GetScreenWidth();
      vidh = GetScreenHeight();
      snprintf(cmd, 200,
               "ffmpeg -f rawvideo -pixel_format rgba -video_size %dx%d "
               "-framerate 6 -i pipe:0 -c:v libx264 out.mp4",
               vidw, vidh);
      ffmpeg_pipe = popen(cmd, "w");
    }
    if (IsKeyPressed(KEY_SPACE)) {
      UnloadTexture(texA);
      texA = fillRedRandom(false);
      BeginTextureMode(rtA);
      DrawTexture(texA, 0, 0, WHITE);
      EndTextureMode();
      BeginTextureMode(rtB);
      DrawTexture(texA, 0, 0, WHITE);
      EndTextureMode();
      updateBrushNoise();
    }

    if (frameCount % BRUSH_NOISE_FRAMES == 0)
      updateBrushNoise();

    pingpongRT();
    if (IsMouseButtonDown(MOUSE_BUTTON_LEFT))
      brushTool(GetMousePosition(), true, 10.0f, BLACK);
    if (IsMouseButtonDown(MOUSE_BUTTON_RIGHT))
      brushTool(GetMousePosition(), false, 10.0f, WHITE);

    frameCount++;

    BeginDrawing();
    DrawTexture(rtA.texture, 0, 0, WHITE);
    // DrawRectangle(0, 0, 100, 40, BLACK);
    // DrawFPS(10, 10);

    EndDrawing();

    if (ffmpeg_pipe) {
      Image frame = LoadImageFromTexture(rtA.texture);
      ImageFlipVertical(&frame); // raylib FBOs are flipped

      fwrite(frame.data, 1, vidw * vidh * 4, ffmpeg_pipe);
      UnloadImage(frame);
    }
  }

  UnloadShader(sh);
  UnloadShader(brushSh);
  UnloadTexture(texA);
  if (brushNoise.id)
    UnloadTexture(brushNoise);
  if (brushNoiseImage.data)
    UnloadImage(brushNoiseImage);
  UnloadRenderTexture(rtA);
  UnloadRenderTexture(rtB);
  CloseWindow();
}
