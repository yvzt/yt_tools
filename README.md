# YT AutoLISP Drafting Tools (v1.0)

A set of AutoLISP commands for producing clean, repeatable **engineering educational figure annotations** (arrows, normal arrows, dimension-like graphics without text, and distributed loads) using **plain AutoCAD geometry** (LINE + LWPOLYLINE).

Version 1.0 introduces **Global Scaling**, **Smart Layering**, and a centralized **Configuration** command.

---

## Acknowledgement

This repository was developed using AI-assisted tools as part of an interactive workflow.

---

## Contents

- `yt_tools.lsp` (Main library)
- **Global Config Command:** `YTCONFIG`
- **Drawing Commands:**
  - `YTARROW`  — Straight arrow (single or double)
  - `YTARROWN` — Arrow normal to curve (Tension/Compression modes)
  - `YTDIMA`   — Dimension style graphic (Arrows, no text)
  - `YTDIM`    — Dimension style graphic (Ticks, no text)
  - `YTDLOAD`  — Linearly varying distributed load

---

## Installation

1. Save `yt_tools.lsp` to a support folder.
2. In AutoCAD:
   - Run `APPLOAD`
   - Select `yt_tools.lsp` and Load.
3. (Optional) Add to the **Startup Suite** content to load automatically.

---

## Global Configuration (`YTCONFIG`)

Type `YTCONFIG` to manage global settings. This replaces the need to set individual variables manually.

### Settings Available:
1.  **Scale:** A global multiplier for all arrows, ticks, and text sizes.
    * *Example:* Set Scale to `100` for site plans, `1.0` for details.
2.  **Layers:** Toggle **Smart Layering** (Auto/Current) and set default colors.
3.  **Params:** Configure base (unscaled) sizes for arrows, ticks, and extensions.
4.  **Reset:** Restore factory defaults.

---

## Smart Layering

The tools operate in two modes (toggle via `YTCONFIG`):

| Mode | Behavior |
| :--- | :--- |
| **Auto (Default)** | Automatically creates and uses semantic layers:<br>• `YT_ANNOTATION` (Color 7) for arrows<br>• `YT_DIM` (Color 8) for dimensions<br>• `YT_LOAD` (Color 1) for loads |
| **Current** | Draws geometry on the currently active layer. Best for quick sketches. |

---

## Command Reference

### 1) `YTARROW` — Straight Arrow

Draws a straight arrow between two points.

**Prompts:**
1. Options: `Length`, `Width` (Change arrowhead size on the fly)
2. Pick tail point
3. Pick head point
4. Double-ended? `[Yes/No]`

**Notes:**
- Checks distance; if points are too close, the arrowhead automatically shrinks to fit.

---

### 2) `YTARROWN` — Normal Arrow (Curve)

Draws an arrow perpendicular to a selected curve (Line, Arc, Spline, Polyline). Supports both **Tension** (Away) and **Pressure** (To) styles.

**Prompts:**
1. **Options Menu:**
   - `Reach`: Length of the arrow shaft (Type value or Pick two points).
   - `To`: Sets direction **To Curve** (Pressure/Compression).
   - `Away`: Sets direction **Away from Curve** (Tension/Normal).
   - `Length` / `Width`: Adjust arrowhead size.
2. Select curve.
3. Pick side point (determines placement).
4. Double-ended? `[Yes/No]`

**Defaults:**
- Default direction: **Away** (Tension).
- Reach defaults to `50.0` (multiplied by Global Scale).

---

### 3) `YTDIMA` — Dimension Graphic (Arrows)

Draws a dimension line with arrowheads inside, extension lines, and ticks. **No text is created.**

**Prompts:**
1. Options: `Length`, `Width` (Adjust arrowhead size)
2. Pick Extension Point 1
3. Pick Extension Point 2
4. Pick Dimension Line Location (Offset)

**Notes:**
- Arrowheads are always drawn **inside** the extension lines.
- Includes small vertical ticks at the corners.

---

### 4) `YTDIM` — Dimension Graphic (Ticks)

Architectural style dimension line with **30° ticks** instead of arrows.

**Prompts:**
1. Options:
   - `ExtLen`: Length of the vertical extension markers.
   - `TickLen`: Length of the slanted tick marks.
2. Pick Extension Point 1
3. Pick Extension Point 2
4. Pick Dimension Line Location (Offset)

**Notes:**
- Ticks are automatically oriented (slanted) based on the dimension direction.

---

### 5) `YTDLOAD` — Distributed Load

Draws a distributed load along a baseline. Arrows are interpolated linearly.

**Workflow:**
1. Pick baseline Start point.
2. Pick baseline End point.
3. Pick Load Direction (side).
4. Enter:
   - Arrow length at Start (`L1`)
   - Arrow length at End (`L2`)
   - Number of divisions (`n`)

**Features:**
- If `L1` or `L2` is **0**, the arrow at that end is omitted.
- Arrows are automatically placed on the `YT_LOAD` layer (Red) if Smart Layering is ON.
- Arrowhead sizes scale with `YTCONFIG` Global Scale.---

---

## License / Usage

Use freely in your own drawings and teaching materials.  
