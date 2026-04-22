#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
#include <stdbool.h>

const float ballr = 12, paddlew = 50, paddlet = 10, margin = 10;
// TODO: spinning balls

Vector2 ball, ballv;
float paddle[2];
float paddlev[2];
float botv = 0;
bool user = false;
int score[2];

bool check_miss() {
  if (ball.x < 0) {
    score[1]++;
    return true;
  }
  if (ball.x > GetScreenWidth()) {
    score[0]++;
    return true;
  }
  return false;
}

void round_reset() {
  user = false;
  paddle[0] = paddle[1] = GetScreenHeight() / 2.0 - paddlew / 2;
  ball = (Vector2){GetScreenHeight() / 2.0, GetScreenHeight() / 2.0};
  ballv = (Vector2){1000, 300};
  paddlev[0] = paddlev[1] = 0;
}

void check_paddle_hit(float paddle_y, int side) {
  if ((side == 0 && ballv.x >= 0) || (side == 1 && ballv.x <= 0))
    return;

  float px = side == 0 ? margin : GetScreenWidth() - margin - paddlet;
  float py = paddle_y;

  float cx = fmaxf(px, fminf(ball.x, px + paddlet));
  float cy = fmaxf(py, fminf(ball.y, py + paddlew));

  float dx = ball.x - cx;
  float dy = ball.y - cy;

  if (dx * dx + dy * dy < ballr * ballr) {
    ballv.x = -ballv.x;
    float offset = (ball.y - (py + paddlew / 2.0f)) / (paddlew / 2.0f);
    ballv.y += offset * 5.0f;
    if (side == 0) {
      ball.x = px + paddlet + ballr + 1.0f;
    } else {
      ball.x = px - ballr - 1.0f;
    }
  }
}

void check_wall_hit() {
  if (ball.y < ballr)
    ballv.y = -ballv.y;
  if (ball.y + ballr > GetScreenHeight())
    ballv.y = -ballv.y;
}

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(144);
  score[0] = score[1] = 0;
  round_reset();

  while (!WindowShouldClose()) {
    if (IsKeyDown(KEY_SPACE))
      round_reset();

    // user paddle
    paddlev[0] = 0;
    if (IsKeyDown(KEY_UP)) {
      paddlev[0] = -5;
      user = true;
    }
    if (IsKeyDown(KEY_DOWN)) {
      paddlev[0] = 5;
      user = true;
    }
    float err;
    if (user) {
      paddle[0] += paddlev[0];
    } else {
      // bot paddles
      err = paddle[0] - (ball.y - paddlew / 2);
      paddlev[0] -= err / 1e1;
      paddle[0] += paddlev[0];
    }
    err = paddle[1] - (ball.y - paddlew / 2);
    paddlev[1] -= err / 1e2;
    paddle[1] += paddlev[1];

    ball.x += GetFrameTime() * ballv.x;
    ball.y += GetFrameTime() * ballv.y;

    check_paddle_hit(paddle[1], 1);
    check_paddle_hit(paddle[0], 0);
    check_wall_hit();
    if (check_miss())
      round_reset();
    BeginDrawing();
    ClearBackground(BLACK);
    DrawRectangle(margin, paddle[0], paddlet, paddlew, WHITE);
    DrawRectangle(GetScreenWidth() - margin - paddlet, paddle[1], paddlet,
                  paddlew, WHITE);
    DrawCircleV(ball, 10, WHITE);
    char scorestr[200];
    snprintf(scorestr, 200, "ball-paddle-game: %d : %d", score[0], score[1]);
    DrawText(scorestr, margin, 10, 20, DARKGREEN);
    DrawText("CONTROLS: UP_ARROW DOWN_ARROW SPACE", margin, 40, 15, DARKGREEN);
    EndDrawing();
  }
  CloseWindow();
}
