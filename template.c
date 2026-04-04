// clang-format off
//DECLS
struct state {
//STRUCTBODY
};
struct prevstate {
//PREVSTRUCTBODY
};
void Init(struct state *c);
bool Step(struct state *s);
int Uninit(struct state *s);
void ReinitAlloc(struct prevstate *s, struct state *t);
void ReinitInPlace(struct prevstate *s, struct state *t);

typedef struct {
  void (*Init)(struct state *);
  bool (*Step)(struct state *);
  int (*Uninit)(struct state *);
  void (*ReinitAlloc)(struct prevstate *, struct state *);
  void (*ReinitInPlace)(struct prevstate *, struct state *);
  unsigned long size, align;
} VTable;
VTable PLUGIN;

// stb style #include "rl_so.c" with one file having #define RL_SO_IMPL
#ifdef RL_SO_IMPL
#include <string.h> // REINIT needs memset
struct state *s_top;
//DEFS

void Init(struct state *s) {
s_top = s;
//INITBODY
}

void ReinitAlloc(struct prevstate *s, struct state *t) {
//REINITALLOCBODY
s_top = (void*)t;
}

void ReinitInPlace(struct prevstate *s, struct state *t) {
//REINITINPLACEBODY
s_top = (void*)t;
}

bool Step(struct state *s) {
//STEPBODY
}

int Uninit(struct state *s) {
//UNINITBODY
}


VTable PLUGIN = (VTable) { &Init, &Step, &Uninit, &ReinitAlloc, &ReinitInPlace, sizeof(struct state), _Alignof(struct state) };
#endif
