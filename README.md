# YT AutoLISP Drafting Tools

A small, fast set of AutoLISP commands for producing clean, repeatable **engineering annotations** (arrows, normal arrows, dimension-like graphics without text, and distributed loads) using **plain AutoCAD geometry** (LINE + LWPOLYLINE), not DIM objects.

This library is intentionally **non-dynamic**: style is fixed per drawing, and drawings remain simple to edit manually.

---

## Acknowledgement

This project was developed with the assistance of AI-based tooling used interactively throughout the design and implementation process.  
The concepts, drafting conventions, and final decisions reflect the author’s own engineering judgement and professional practice.

---
## Contents

- `yt_tools.lsp` (main tool file)
- Commands:
  - `YTARROW`  — straight arrow between two points
  - `YTARROWN` — arrow normal to a selected curve at a chosen side
  - `YTDIMA`   — aligned-dimension-like (no text), **arrows always inside**
  - `YTDIM`    — aligned-dimension-like (no text), **ticks at ends (30°)**
  - `YTDLOAD`  — linearly varying distributed load

---

## Installation

1. Place `yt_tools.lsp` somewhere on your machine.
2. In AutoCAD:
   - Run `APPLOAD`
   - Load `yt_tools.lsp`
3. (Optional) Add to Startup Suite so it loads automatically.

---

## General Conventions

- Geometry is created using:
  - **LINE** for shafts/extension markers/ticks
  - **LWPOLYLINE** for wedge arrowheads
- Tools use persistent global defaults stored in variables like:
  - `*yt_arrow_len*`, `*yt_arrow_w*`, etc.
- Most commands follow the pattern:
  1) **Options** (set defaults)  
  2) **Pick geometry**  
  3) **Draw**

---

## Command Reference

### 1) `YTARROW` — Straight Arrow

Draws a straight arrow between two points:
- By default: head at the **second** point
- Optional: **both-end** arrow

**Prompts**
1. Options: `Length`, `Width`
2. Pick first point
3. Pick second point (head)
4. Both-end? `[Yes/No]`

**Defaults**
- `*yt_arrow_len*` = `7.0` (arrowhead length)
- `*yt_arrow_w*`   = `2.0` (half-width parameter; wedge base width ≈ `2*w`)

**Notes**
- If the segment is too short, the tool automatically reduces head length to avoid overlap.

---

### 2) `YTARROWN` — Arrow Normal to Curve

Draws an arrow **normal** to a selected object at the closest point to your side-pick.

**Supported objects**
- LINE, ARC, CIRCLE
- LWPOLYLINE / POLYLINE
- SPLINE
- (any AutoCAD curve supported by `vlax-curve-*`)

**Prompts**
1. Options: `HeadLen`, `Width`, `Reach`
2. Select curve
3. Pick point near curve (defines which side)
4. Both-end? `[Yes/No]`

**Defaults**
- `*yt_n_head_len*` = `7.0`
- `*yt_n_head_w*`   = `2.0`
- `*yt_n_reach*`    = `50.0` (distance from curve point to arrow tip)

**Notes**
- The side pick determines the normal direction (left/right of tangent).
- For both-end, a symmetric arrow is drawn on both sides of the curve point.

---

### 3) `YTDIMA` — Aligned Dimension Geometry (No Text, Arrows Inside)

Produces a dimension-like graphic without text:
- **shaft line** between two tips
- simple **extension markers** at ends (short, centered)
- **arrowheads always inside** (triangle body extends inward)

**Workflow**
1. Pick point 1
2. Pick point 2
3. Pick “dimension line location” (defines offset side and distance)

**Defaults**
- `*yt_dima_head_len*` = `7.0`
- `*yt_dima_head_w*`   = `2.0`

**Notes**
- This is intentionally simplified: extension markers are short and meant for quick manual adjustment.
- In “minor cases”, you can mirror the result manually if you want outside arrows.

---

### 4) `YTDIM` — Aligned Dimension Geometry (No Text, No Arrows, 30° Ticks)

Like `YTDIMA` but:
- **no arrowheads**
- adds **ticks** at both ends
- each tick:
  - is centered at the shaft endpoint
  - makes **30°** with the extension marker direction
  - tick length = **half** the extension marker length

**Defaults**
- `*yt_dim_ext_len*` = `7.0` (extension marker length)
- `*yt_dim_tick_ang*` = `30.0` (tick angle; currently fixed by design)

**Notes**
- This is meant as a “clean drafting symbol” alternative to arrowheads.

---

### 5) `YTDLOAD` — Linearly Varying Distributed Load

Draws a distributed load along a baseline (two points), with arrows on a selected side.
Arrow lengths vary **linearly** from `L1` at start to `L2` at end.

**Workflow**
1. Pick baseline start point
2. Pick baseline end point
3. Pick side (near the baseline)
4. Enter:
   - `L1` arrow length at start (can be **0**)
   - `L2` arrow length at end (can be **0**)
   - `n` number of divisions

**Arrow count logic**
- Normally arrows are placed at division points `i = 0..n` → **n+1 arrows**
- If `L1 = 0` → no arrow at the start point
- If `L2 = 0` → no arrow at the end point
- (intermediate arrows are still drawn as per linear interpolation)

**Defaults**
- `*yt_load_head_len*` = `7.0`
- `*yt_load_head_w*`   = `2.0`

**Important implementation note**
- Do not use `t` as a variable name in AutoLISP (it conflicts with `T`).
  This tool uses `s` as the interpolation fraction.

---

## Style / Consistency Strategy (Recommended)

These tools are designed so that you can decide a **fixed style per drawing**.

A practical workflow:
- Choose arrowhead length/width at the beginning for the figure scale.
- Keep the same parameters for the entire figure.
- If you need global retuning later, implement a “consistency” command that:
  - selects objects by layer or selection set
  - updates widths/lengths accordingly

(Keeping geometry simple is the priority.)

---

## License / Usage

Use freely in your own drawings and teaching materials.  
If you publish or share modifications, consider keeping the command names consistent (YT prefix) for compatibility.

---

## Roadmap (Optional)

- `YTSTYLE` / `YTCONS`: one-shot consistency tool to retune arrowheads and shafts in a selected set.
- Optional “envelope line” for `YTDLOAD` (connect load arrow tails to depict triangular/trapezoidal load surfaces).
- Tick style variants (ISO 45°, perpendicular caps, etc.) for `YTDIM`.

---
