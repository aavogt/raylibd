// DO NOT EDIT
// raylibd generates dll.c from main.c
// clang-format off
#include <sys/stat.h>
#include <string.h>

// --- Asset hot-reload infrastructure ---
#define _RL_MAX_ASSET_PATHS 4
#define _RL_MAX_ASSETS 64

typedef struct {
  const char *paths[_RL_MAX_ASSET_PATHS]; // file path args (strdup'd)
  int kind;              // index into _rl_asset_funcs
  int field_offset;      // byte offset of the result field in struct state
  int field_size;        // sizeof the result field
  struct timespec mtime[_RL_MAX_ASSET_PATHS];
} AssetSlot;

//DECLS
struct state {
//STRUCTBODY
AssetSlot _assets[_RL_MAX_ASSETS];
int _nassets;
};
struct prevstate {
//PREVSTRUCTBODY
AssetSlot _assets[_RL_MAX_ASSETS];
int _nassets;
};
void Init(struct state *c);
bool Step(struct state *s);
int Uninit(struct state *s);
void ReinitAlloc(struct prevstate *s, struct state *t);
void ReinitInPlace(struct prevstate *s, struct state *t);
void ReloadAssets(struct state *s);

typedef struct {
  void (*Init)(struct state *);
  bool (*Step)(struct state *);
  int (*Uninit)(struct state *);
  void (*ReinitAlloc)(struct prevstate *, struct state *);
  void (*ReinitInPlace)(struct prevstate *, struct state *);
  void (*ReloadAssets)(struct state *);
  unsigned long size, align;
} VTable;
VTable PLUGIN;

// stb style #include "rl_so.c" with one file having #define RL_SO_IMPL
#ifdef RL_SO_IMPL
#include <string.h> // REINIT needs memset
#include <stdio.h>
#include <stddef.h> // offsetof for ASSETWRAPPERS by hs/Transform/Assets/Macro.hs

struct state *s_top;

// -- asset slot helpers --
static int _rl_asset_mtime_changed(AssetSlot *slot) {
  for (int i = 0; i < _RL_MAX_ASSET_PATHS; i++) {
    if (!slot->paths[i]) continue;
    struct stat st;
    if (stat(slot->paths[i], &st)) continue;
    struct timespec cur = st.st_mtim;
    if (cur.tv_sec > slot->mtime[i].tv_sec ||
        (cur.tv_sec == slot->mtime[i].tv_sec &&
         cur.tv_nsec > slot->mtime[i].tv_nsec)) {
      slot->mtime[i] = cur;
      return 1;
    }
  }
  return 0;
}

// asset wrapper: strdup paths, record mtime, register in s_top->_assets
static int _rl_register_asset(int kind, int field_offset, int field_size,
                               int npaths, const char **paths) {
  if (!s_top || s_top->_nassets >= _RL_MAX_ASSETS) return -1;
  int idx = s_top->_nassets++;
  AssetSlot *slot = &s_top->_assets[idx];
  memset(slot, 0, sizeof(*slot));
  slot->kind = kind;
  slot->field_offset = field_offset;
  slot->field_size = field_size;
  for (int i = 0; i < npaths && i < _RL_MAX_ASSET_PATHS; i++) {
    if (paths[i]) {
      slot->paths[i] = strdup(paths[i]);
      struct stat st;
      if (stat(paths[i], &st) == 0)
        slot->mtime[i] = st.st_mtim;
    }
  }
  return idx;
}

// Generic register helpers as GCC statement-expression macros. -- _rl_register_asset_N: N = number of path args to register.
#define _rl_register_asset_1(kind, offset, call, path0) \
  ({ __typeof__(call) _r = (call); \
     if (s_top && s_top->_nassets < _RL_MAX_ASSETS) { \
       const char *_ps[] = { (path0) }; \
       _rl_register_asset((kind), (offset), sizeof(_r), 1, _ps); \
     } _r; })

#define _rl_register_asset_2(kind, offset, call, path0, path1) \
  ({ __typeof__(call) _r = (call); \
      if (s_top && s_top->_nassets < _RL_MAX_ASSETS) { \
        const char *_ps[] = { (path0), (path1) }; \
        _rl_register_asset((kind), (offset), sizeof(_r), 2, _ps); \
      } _r; })
//ASSETWRAPPERS

//DEFS

void Init(struct state *s) {
s_top = s;
s->_nassets = 0;
//INITBODY
}

void ReinitAlloc(struct prevstate *s, struct state *t) {
//REINITALLOCBODY
// carry over asset registry
t->_nassets = s->_nassets;
memcpy(t->_assets, s->_assets, sizeof(s->_assets));
s_top = (void*)t;
}

void ReinitInPlace(struct prevstate *s, struct state *t) {
//REINITINPLACEBODY
// asset slots are in-place, no copy needed
s_top = (void*)t;
}

bool Step(struct state *s) {
//STEPBODY
}

int Uninit(struct state *s) {
//UNINITBODY
}

void ReloadAssets(struct state *s) {
  const char **args;
  void *dest;
  for (int _i = 0; _i < s->_nassets; _i++) {
     args = s->_assets[_i].paths;
     if (_rl_asset_mtime_changed(&s->_assets[_i])) {
       printf("asset changed: %s\n",
              s->_assets[_i].paths[0] ? s->_assets[_i].paths[0] : "(null)");
       switch (s->_assets[_i].kind) {
//ASSETRELOADSWITCHKIND
        };
    // AssetFunc "LoadSound"       "UnloadSound"       1 1,
    // AssetFunc "LoadWave"        "UnloadWave"        1 1,
    // AssetFunc "LoadMusicStream" "UnloadMusicStream" 1 1
     };
  }; 
  }


VTable PLUGIN = (VTable) { &Init, &Step, &Uninit, &ReinitAlloc, &ReinitInPlace, &ReloadAssets, sizeof(struct state), _Alignof(struct state) };
#endif
