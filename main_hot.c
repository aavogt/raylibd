#include "dll.c"
#include <dlfcn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

const int MTIME_FRAMES = 30;
typedef struct {
  void *dl;
  VTable *api;
} Plugin;
Plugin p = {0};

static time_t mtime(const char *p) {
  struct stat st;
  return stat(p, &st) == 0 ? st.st_mtime : 0;
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
  time_t last = mtime("dll.so");
  while (p.api->Step(&s)) {
    if (frame++ >= MTIME_FRAMES) {
      time_t t = mtime("dll.so");
      if (t != last) {
        printf("main_hot reloaded\n");
        plugin_unload();
        plugin_load();
        last = mtime("dll.so");

        p.api->Reinit(&s);
      }
      frame = 0;
    }
  }

  p.api->Uninit(&s);
  plugin_unload();
  return 0;
}
