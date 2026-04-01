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
void Reinit(struct prevstate *s, struct state *t);

typedef struct {
  void (*Init)(struct state *);
  bool (*Step)(struct state *);
  int (*Uninit)(struct state *);
  void (*Reinit)(struct prevstate *, struct state *);
  unsigned long size, align;
} VTable;
VTable PLUGIN;

// stb style #include "rl_so.c" with one file having #define RL_SO_IMPL
#ifdef RL_SO_IMPL
struct state *s_top;
//DEFS

void Init(struct state *s) {
s_top = s;
//INITBODY
}

void Reinit(struct prevstate *s, struct state *t) {
//REINITBODY
if (t) {
s_top = (void*)t;
} else {
s_top = (void*)s;
}
}

bool Step(struct state *s) {
//STEPBODY
}

int Uninit(struct state *s) {
//UNINITBODY
}


VTable PLUGIN = (VTable) { &Init, &Step, &Uninit, &Reinit, sizeof(struct state), _Alignof(struct state) };
#endif
