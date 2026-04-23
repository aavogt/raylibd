#ifndef RAYLIBD
#include "raygui.h"
#include "raylib.h"
#include "raymath.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#endif
#include <stdbool.h>

// 1=x 2=o, 0 empty or draw
int board[3][3], score[3];
bool xturn = true;

// visual
int h, w, bh, bw;
const int b = 20;

// defined below main
void reset_board();
int check_winner();
bool any_cells_open();
void drawcell(int, int, const char *, Color);

int main() {
  SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
  InitWindow(800, 600, "main.c");
  SetTargetFPS(144);
  while (!WindowShouldClose()) {
    h = GetScreenHeight();
    w = GetScreenWidth();
    bh = h - 2 * b;
    bw = w - 2 * b;
    BeginDrawing();
    ClearBackground(BLACK);
    DrawLine(b + bw / 3, b, b + bw / 3, bh + b, RAYWHITE);
    DrawLine(b + 2 * bw / 3, b, b + 2 * bw / 3, bh + b, RAYWHITE);
    DrawLine(b, b + bh / 3, bw + b, b + bh / 3, RAYWHITE);
    DrawLine(b, b + 2 * bh / 3, bw + b, b + 2 * bh / 3, RAYWHITE);

    if (IsKeyPressed(KEY_SPACE))
      reset_board();

    switch (check_winner()) {
    case 2:
      score[2] += 1;
      reset_board();
      break;
    case 1:
      score[1] += 1;
      reset_board();
      break;
    default:
      if (!any_cells_open()) {
        score[0] += 1;
        reset_board();
      }
    }

    Vector2 p = GetMousePosition();
    int i = (p.x - b) / (bw / 3);
    int j = (p.y - b) / (bh / 3);
    bool onBoard = p.x >= b && p.x < b + bw && p.y >= b && p.y < b + bh &&
                   i >= 0 && i < 3 && j >= 0 && j < 3;

    if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) && onBoard)
      board[i][j] = 0;

    if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON) && onBoard) {
      board[i][j] = 1 + xturn;
      xturn = !xturn;
    }

    if (onBoard && board[i][j] == 0)
      drawcell(i, j, xturn ? "x" : "o", xturn ? DARKPURPLE : DARKBLUE);

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) {
        if (board[i][j] > 0) {
          const char *mark = board[i][j] > 1 ? "x" : "o";
          drawcell(i, j, mark, board[i][j] > 1 ? PURPLE : BLUE);
        }
      }

    char scorestr[30];
    Color xColor = xturn ? RAYWHITE : DARKGRAY;
    Color oColor = xturn ? DARKGRAY : RAYWHITE;

    snprintf(scorestr, sizeof(scorestr), "X - %d", score[2]);
    DrawText(scorestr, 10, 10, 20, xColor);

    snprintf(scorestr, sizeof(scorestr), "O - %d", score[1]);
    DrawText(scorestr, 10, 34, 20, oColor);

    snprintf(scorestr, sizeof(scorestr), "Draws - %d", score[0]);
    DrawText(scorestr, 10, 58, 20, DARKGRAY);
    EndDrawing();
  }
  CloseWindow();
}

void reset_board() {
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
      board[i][j] = 0;
}

int check_winner() {
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

bool any_cells_open() {
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
      if (board[i][j] == 0)
        return true;
  return false;
}

void drawcell(int i, int j, const char *mark, Color color) {
  int cellW = bw / 3;
  int cellH = bh / 3;
  int fontSize = 1.5f * cellH;
  int textW = MeasureText(mark, fontSize);

  int x = b + i * cellW + (cellW - textW) / 2;
  int y = b + j * cellH + (cellH - fontSize) / 2;

  DrawText(mark, x, y, fontSize, color);
}
