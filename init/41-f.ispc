// sample/41-f.ispc
// Pair with sample/41-ispc.c

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

// Rasterize voxels into an RGBA8 pixel buffer using orthographic projection.
// No depth test; last write wins. Camera is defined by position, right/up vectors
// and an ortho_size which describes half-height in world units.
export void fill_texture_ortho(uniform int ni,
                               uniform float den,
                               uniform int screenW,
                               uniform int screenH,
                               uniform float camX, uniform float camY, uniform float camZ,
                               uniform float rightX, uniform float rightY, uniform float rightZ,
                               uniform float upX, uniform float upY, uniform float upZ,
                               uniform float ortho_size,
                               uniform uint8 out_pixels[],
                               uniform int out_len) {
  uniform float dx = (float)ni * 0.5f;

  // Map NDC-like coordinates: pixel center (px + 0.5)/W -> [-1,1] horizontally and vertically
  foreach (i = 0 ... ni, j = 0 ... ni, k = 0 ... ni) {
    varying float vx = ((varying float)i - dx) / den;
    varying float vy = ((varying float)j - dx) / den;
    varying float vz = ((varying float)k - dx) / den;

    varying int cidx = palette_index(scene(vx, vy, vz));
    if (cidx < 0) continue;

    // World position
    varying float wx = vx;
    varying float wy = vy;
    varying float wz = vz;

    // Compute vector from camera to world point
    varying float vx_cam = wx - camX;
    varying float vy_cam = wy - camY;
    varying float vz_cam = wz - camZ;

    // Project onto camera axes: right and up
    varying float px_world = vx_cam * rightX + vy_cam * rightY + vz_cam * rightZ;
    varying float py_world = vx_cam * upX + vy_cam * upY + vz_cam * upZ;

    // Convert to pixel coordinates (centered)
    varying float ndc_x = px_world / ortho_size; // in [-1,1] (approx)
    varying float ndc_y = py_world / ortho_size; // in [-1,1]

    varying float fx = (ndc_x * 0.5f + 0.5f) * (float)screenW;
    varying float fy = (1.0f - (ndc_y * 0.5f + 0.5f)) * (float)screenH; // flip Y for image coords

    varying int ix = (int)fx;
    varying int iy = (int)fy;

    if (ix < 0 || ix >= screenW || iy < 0 || iy >= screenH) continue;

    varying int pidx = (iy * screenW + ix) * 4;
    if (pidx >= out_len - 4) continue;

    // simple palette to RGBA mapping (alpha-ish)
    varying int vr = (cidx == 0) ? 255 : 0;
    varying int vg = (cidx == 1) ? 255 : 0;
    varying int vb = (cidx == 2) ? 255 : 0;
    varying int va = 128;

    out_pixels[pidx + 0] = (uint8)vr;
    out_pixels[pidx + 1] = (uint8)vg;
    out_pixels[pidx + 2] = (uint8)vb;
    out_pixels[pidx + 3] = (uint8)va;
  }
}
