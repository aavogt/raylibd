// clang-format off
struct state {
//STRUCTBODY
};
void Init(struct state *c);
bool Step(struct state *s);
int Uninit(struct state *s);
void Reinit(struct state *s, struct state *t);

typedef struct {
  void (*Init)(struct state *);
  bool (*Step)(struct state *);
  int (*Uninit)(struct state *);
  void (*Reinit)(struct state *, struct state *);
  unsigned long size, align;
} VTable;
VTable PLUGIN;

// stb style #include "rl_so.c" with one file having #define RL_SO_IMPL
#ifdef RL_SO_IMPL
void Init(struct state *s) {
//INITBODY
}

void Reinit(struct state *s, struct state *t) {
//REINITBODY
}

bool Step(struct state *s) {
  static bool reinited = false;
  if (!reinited) {
    Reinit(s);
    reinited = true;
  }
//STEPBODY
}

int Uninit(struct state *s) {
//UNINITBODY
}


VTable PLUGIN = (VTable) { &Init, &Step, &Uninit, &Reinit, sizeof(struct state), _Alignof(struct state) };
#endif
