#include "raylib.h"
#include <stdbool.h>
#include <stdio.h>
// clang-format off
struct state {
//STRUCTBODY
};
void Init(struct state *c);
bool Update(struct state *s);
int Uninit(struct state *s);

// stb style #include "rl_so.c" with one file having #define RL_SO_IMPL
#ifdef RL_SO_IMPL
void Init(struct state *s) {
//INITBODY
}

bool Step(struct state *s) {
//STEPBODY
}

int Uninit(struct state *s) {
//UNINITBODY
}
#endif
