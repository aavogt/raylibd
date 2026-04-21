#version 330

in vec2 fragTexCoord;
out vec4 fragColor;

uniform sampler2D texture0;
uniform vec2      resolution;

// two activator-inhibitor processes
float actRadius1 = 1.5;
float inhRadius1 = 3.0;
float actRadius2 = 4.0;
float inhRadius2 = 6.0;
float stepSize = 0.01;

float circleAvg(vec2 uv, float radius) {
    vec2 px = 1.0 / resolution;
    int r = int(ceil(radius));
    float sum = 0.0, count = 0.0;
    for (int y = -r; y <= r; y++) {
        for (int x = -r; x <= r; x++) {
            if (float(x*x + y*y) <= radius * radius) {
                sum += texture(texture0, uv + vec2(x, y) * px).r;
                count += 1.0;
            }
        }
    }
    return sum / max(count, 1.0);
}

void main() {
    vec2 uv  = fragTexCoord;
    float v  = texture(texture0, uv).r;

    float a1 = circleAvg(uv, actRadius1);
    float i1 = circleAvg(uv, inhRadius1);
    float a2 = circleAvg(uv, actRadius2);
    float i2 = circleAvg(uv, inhRadius2);

    // Scale whose activator/inhibitor are closest "wins"
    float var1 = abs(a1 - i1);
    float var2 = abs(a2 - i2);

    float best_act, best_inh;
    if (var1 < var2) { best_act = a1; best_inh = i1; }
    else             { best_act = a2; best_inh = i2; }

    v += (best_act > best_inh) ? stepSize : -stepSize;
    float min = 0;
    float max = 1.0;
    v  = clamp(v, min, max);

    fragColor = vec4((v - (max+min)/2) / (max-min) + 0.5,   // remap [-1,1] -> [0,1] for display
                     (v - (max+min)/2) / (max-min) + 0.5,
                     (v - (max+min)/2) / (max-min) + 0.5,
                     1.0);
}
