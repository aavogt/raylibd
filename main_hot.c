#include "dll.c"
#include <dlfcn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

const int MTIME_SKIP_FRAMES = 0;
typedef struct {
  void *dl;
  VTable *api;
} Plugin;
Plugin p = {0};

bool mtime_changed(const char *p) {
  static struct timespec prev;
  struct timespec cur;
  static ulong lastns;
  static bool have_prev = false;
  struct stat st;

  if (stat(p, &st))
    return false;

  cur = st.st_mtim;
  if (have_prev) {
    bool r = cur.tv_sec > prev.tv_sec ||
             (cur.tv_sec == prev.tv_sec && cur.tv_nsec > prev.tv_nsec);
    prev = cur;
    return r;
  } else {
    have_prev = true;
    prev = cur;
    return true;
  }
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
  struct state s = {0};

  plugin_load();
  p.api->Init(&s);

  int frame = 0;
  while (p.api->Step(&s)) {
    if (frame++ >= MTIME_SKIP_FRAMES) {
      if (mtime_changed("dll.so")) {
        printf("main_hot reloaded\n");
        plugin_unload();
        plugin_load();

        p.api->Reinit(&s);
      }
      frame = 0;
    }
  }

  p.api->Uninit(&s);
  plugin_unload();
  return 0;
}
