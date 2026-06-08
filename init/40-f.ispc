// sample/40-f.ispc
// Pair with sample/40-ispc.c

export void simple(uniform float vin[], uniform float vout[], uniform int n) {
  foreach (i = 0 ... n) {
    vout[i] = vin[i] * vin[i];
  }
}

static inline varying float sdf_box(varying float x, varying float y, varying float z) {
  uniform float sx = 3.0f, sy = 1.0f, sz = 1.0f;

  varying float qx = abs(x), qy = abs(y), qz = abs(z);
  varying float dx = qx - sx, dy = qy - sy, dz = qz - sz;

  varying float distX = max(dx, 0.0f);
  varying float distY = max(dy, 0.0f);
  varying float distZ = max(dz, 0.0f);

  varying float dist = sqrt(distX * distX + distY * distY + distZ * distZ);
  varying float dm = max(max(dx, dy), dz);
  return dist + dm;
}

static inline varying float sdf_ball(varying float x, varying float y, varying float z) {
  uniform float r = 2.0f;
  varying float R = sqrt(x * x + y * y + z * z);
  return R - r;
}

static inline varying float scene(varying float x, varying float y, varying float z) {
  varying float f = sdf_ball(x, y, z);
  varying float g = sdf_box(x, y, z);
  return f + g - sqrt(g * g + f * f);
}

static inline varying int palette_index(varying float d) {
  varying float a = abs(d);
  if (a < 0.33f) return 0;
  if (a < 0.66f) return 1;
  if (a < 1.00f) return 2;
  return -1;
}

// Dense output: out[idx] in {-1,0,1,2}
// idx layout: ((i * ni) + j) * ni + k
export void fill_scene_dense(uniform int ni,
                             uniform float den,
                             uniform int8 out[],
                             uniform int out_len) {
  uniform float dx = (float)ni * 0.5f;

  foreach (i = 0 ... ni, j = 0 ... ni, k = 0 ... ni) {
    varying float x = ((varying float)i - dx) / den;
    varying float y = ((varying float)j - dx) / den;
    varying float z = ((varying float)k - dx) / den;

    varying int c = palette_index(scene(x, y, z));
    varying int idx = ((i * ni) + j) * ni + k;
    if (idx < out_len) out[idx] = (int8)c;
  }
}
