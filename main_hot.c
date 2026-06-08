// main_hot is part of raylibd
// edit main.c instead
#include "dll.c"
#include <dlfcn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define MIN(a, b)                                                              \
  ({                                                                           \
    __typeof__(a) _a = (a);                                                    \
    __typeof__(b) _b = (b);                                                    \
    _a < _b ? _a : _b;                                                         \
  })

const int MTIME_SKIP_FRAMES = 0;
typedef struct {
  void *dl;
  VTable *api;
} Plugin;
Plugin p = {0};

const int ASSET_POLL_FRAMES = 1; // check assets every N frames
#define MAX_WATCHED_SOS 128

typedef struct {
  const char *path;
  struct timespec prev;
  bool have_prev;
} MTimeEntry;

static MTimeEntry mtime_entries[MAX_WATCHED_SOS];
static int mtime_entries_n = 0;

static MTimeEntry *mtime_entry_get(const char *path) {
  for (int i = 0; i < mtime_entries_n; i++) {
    if (strcmp(mtime_entries[i].path, path) == 0)
      return &mtime_entries[i];
  }
  if (mtime_entries_n >= MAX_WATCHED_SOS)
    return NULL;
  mtime_entries[mtime_entries_n] = (MTimeEntry){.path = path};
  return &mtime_entries[mtime_entries_n++];
}

static bool mtime_changed_path(const char *p) {
  struct stat st;
  if (stat(p, &st))
    return false;

  MTimeEntry *entry = mtime_entry_get(p);
  if (!entry)
    return false;

  struct timespec cur = st.st_mtim;
  if (entry->have_prev) {
    bool r = cur.tv_sec > entry->prev.tv_sec ||
             (cur.tv_sec == entry->prev.tv_sec &&
              cur.tv_nsec > entry->prev.tv_nsec);
    entry->prev = cur;
    return r;
  }

  entry->have_prev = true;
  entry->prev = cur;
  return false;
}

static bool any_plugin_changed(void) {
  if (mtime_changed_path("dll.so"))
    return true;
  for (int i = 0; i < RL_ISPC_SO_FILES_COUNT; i++) {
    const char *path = RL_ISPC_SO_FILES[i];
    if (path && mtime_changed_path(path))
      return true;
  }
  return false;
}

void plugin_load() {
  p.dl = dlopen("dll.so", RTLD_NOW | RTLD_GLOBAL);
  if (!p.dl) {
    fprintf(stderr, "dlopen: %s\n", dlerror());
    exit(1);
  }

  p.api = (VTable *)dlsym(p.dl, "PLUGIN");
  if (!p.api) {
    fprintf(stderr, "dlsym PLUGIN: %s\n", dlerror());
    exit(1);
  }
}

static void plugin_unload() {
  if (p.dl)
    dlclose(p.dl);
  p = (Plugin){0};
}

int main(int argc, char **argv) {
  plugin_load();

  unsigned long capacity = 2 * p.api->size;
  struct state *s = aligned_alloc(p.api->align, capacity);
  memset(s, 0, capacity);
  p.api->Init(s);

  int frame = 0;
  int asset_frame = 0;
  while (p.api->Step(s)) {
    // poll asset mtimes
    if (asset_frame++ >= ASSET_POLL_FRAMES) {
      p.api->ReloadAssets(s);
      asset_frame = 0;
    }
    if (frame++ >= MTIME_SKIP_FRAMES) {
      if (any_plugin_changed()) {
        printf("main_hot reloaded\n");

        Plugin q = p;
        plugin_unload();
        plugin_load();

        if (q.api->align != p.api->align || p.api->size >= capacity) {
          unsigned long capacity_prev = capacity;
          capacity = p.api->size * 2;
          printf("growing state from %ld to %ld \n", capacity_prev, capacity);
          void *t = aligned_alloc(p.api->align, capacity);
          memset(t, 0, capacity);
          memcpy(t, s, MIN(p.api->size, q.api->size));
          p.api->ReinitAlloc((struct prevstate *)s, t);
          free(s);
          s = t;
        } else
          p.api->ReinitInPlace((struct prevstate *)s, s);
      }
      frame = 0;
    }
  }

  p.api->Uninit(s);
  plugin_unload();
  return 0;
}
