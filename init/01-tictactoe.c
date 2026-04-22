#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
#include <stdbool.h>

int board[3][3], h, w;
const int frame = 20;
int score[3] = {0, 0, 0};

bool xturn = true;

static void reset_board(void) {
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
      board[i][j] = 0;
}

static int check_winner(void) {
  for (int i = 0; i < 3; i++) {
    if (board[i][0] != 0 && board[i][0] == board[i][1] &&
        board[i][1] == board[i][2])
      return board[i][0];
    if (board[0][i] != 0 && board[0][i] == board[1][i] &&
        board[1][i] == board[2][i])
      return board[0][i];
  }

  if (board[0][0] != 0 && board[0][0] == board[1][1] &&
      board[1][1] == board[2][2])
    return board[0][0];
  if (board[2][0] != 0 && board[2][0] == board[1][1] &&
      board[1][1] == board[0][2])
    return board[2][0];

  return 0;
}

// could be a bit more eager
static bool is_draw(void) {
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
      if (board[i][j] == 0)
        return false;
  return check_winner() == 0;
}

static void drawcell(int i, int j, const char *mark, Color color) {
  int bw = w - 2 * frame;
  int bh = h - 2 * frame;
  int cellW = bw / 3;
  int cellH = bh / 3;
  int fontSize = (int)(1.5f * cellH);
  int textW = MeasureText(mark, fontSize);

  int x = frame + i * cellW + (cellW - textW) / 2;
  int y = frame + j * cellH + (cellH - fontSize) / 2;

  DrawText(mark, x, y, fontSize, color);
}

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(144);
  while (!WindowShouldClose()) {
    h = GetScreenHeight();
    w = GetScreenWidth();
    int bh = h - 2 * frame;
    int bw = w - 2 * frame;
    BeginDrawing();
    ClearBackground(BLACK);
    DrawLine(frame + bw / 3, frame, frame + bw / 3, bh + frame, RAYWHITE);
    DrawLine(frame + 2 * bw / 3, frame, frame + 2 * bw / 3, bh + frame,
             RAYWHITE);
    DrawLine(frame, frame + bh / 3, bw + frame, frame + bh / 3, RAYWHITE);
    DrawLine(frame, frame + 2 * bh / 3, bw + frame, frame + 2 * bh / 3,
             RAYWHITE);

    if (IsKeyPressed(KEY_SPACE)) {
      reset_board();
    }
    if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) {
      Vector2 p = GetMousePosition();
      int i = (p.x - frame) / (w / 3);
      int j = (p.y - frame) / (h / 3);
      board[i][j] = 0;
    }
    if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
      Vector2 p = GetMousePosition();
      int i = (p.x - frame) / (w / 3);
      int j = (p.y - frame) / (h / 3);
      board[i][j] = 1 + xturn;
      xturn = !xturn;
    }

    int winner = check_winner();
    if (winner != 0) {
      if (winner == 2)
        score[0] += 1;
      else
        score[1] += 1;
      reset_board();
    } else if (is_draw()) {
      score[2] += 1;
      reset_board();
    }

    Vector2 hover = GetMousePosition();
    int hoverI = (hover.x - frame) / (bw / 3);
    int hoverJ = (hover.y - frame) / (bh / 3);
    bool hoverInBounds = hover.x >= frame && hover.x < frame + bw &&
                         hover.y >= frame && hover.y < frame + bh &&
                         hoverI >= 0 && hoverI < 3 && hoverJ >= 0 && hoverJ < 3;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) {
        if (board[i][j] > 0) {
          const char *mark = board[i][j] > 1 ? "x" : "o";
          drawcell(i, j, mark, board[i][j] > 1 ? PURPLE : BLUE);
        } else if (hoverInBounds && i == hoverI && j == hoverJ) {
          const char *mark = xturn ? "x" : "o";
          drawcell(i, j, mark, xturn ? DARKPURPLE : DARKBLUE);
        }
      }

    char scorestr[30];
    Color xColor = xturn ? RAYWHITE : DARKGRAY;
    Color oColor = xturn ? DARKGRAY : RAYWHITE;

    snprintf(scorestr, sizeof(scorestr), "X - %d", score[0]);
    DrawText(scorestr, 10, 10, 20, xColor);

    snprintf(scorestr, sizeof(scorestr), "O - %d", score[1]);
    DrawText(scorestr, 10, 34, 20, oColor);

    snprintf(scorestr, sizeof(scorestr), "Draws - %d", score[2]);
    DrawText(scorestr, 10, 58, 20, DARKGRAY);
    EndDrawing();
  }
  CloseWindow();
}
