#import "@preview/fletcher:0.5.8" as fletcher: diagram, edge, node
#set text(font: "TeX Gyre Pagella", size: 12pt, spacing: 100%)
#show math.equation: set text(font: "TeX Gyre Pagella Math")
#set page(width: 9in, height: 3.75in)

// --- Helper Functions ---
// Enforce a fixed width so the left edges align perfectly when Fletcher centers the nodes
#let row-label(content) = box(width: 11em, align(left, content))

// Uses string identifiers and native Typst shapes for the data blocks
#let draw-shape(s, f) = {
  if s == "circle" {
    circle(radius: 0.5em, fill: f, stroke: 0.5pt)
  } else if s == "rect" {
    rect(width: 1em, height: 1em, radius: 2pt, fill: f, stroke: 0.5pt)
  } else if s == "triangle" {
    polygon.regular(size: 1.2em, vertices: 3, fill: f, stroke: 0.5pt)
  } else if s == "diamond" {
    rotate(45deg, rect(width: 0.75em, height: 0.75em, fill: f, stroke: 0.5pt))
  }
}

#let group-shapes(s) = stack(
  dir: ltr,
  spacing: 2pt,
  draw-shape(s, white),
  draw-shape(s, luma(60%)),
  draw-shape(s, black),
)

#align(center + horizon)[
  #diagram(
    spacing: (0.5em, 0.5em),
    node-stroke: none,
    // --- Row Labels (Fixed Alignment) ---
    node((0.25, -1), row-label[*Level 4:* \ Hyperparameters]),
    node((0.25, 0), row-label[*Level 3:* \ Abstract Category Knowledge]),
    node((0.25, 1.5), row-label[*Level 2:*\ Category Means]),
    node((0.25, 3), row-label[*Level 1:*\ Data]),
    node((0.25, 4), row-label[*Observations*]),

    // ==========================================
    // --- DIMENSION 1: SHAPE ---
    // ==========================================
    node((2.5, -2), [*Shape Dimension* ($m=1$)], name: <dim_s>),

    node((2.5, -1), $lambda_1$, name: <hyperprior_s>),
    edge(<hyperprior_s>, <hyp_s>, "-|>"),

    // Level 3: Hyperparameters
    node((2.5, 0), $alpha_1, bold(beta)_1$, name: <hyp_s>),

    // Level 2: Thetas
    node((1, 1.5), $bold(theta)_(11)$, name: <t_s1>),
    node((2, 1.5), $bold(theta)_(21)$, name: <t_s2>),
    node((3, 1.5), $bold(theta)_(31)$, name: <t_s3>),
    node((4, 1.5), $bold(theta)_(41)$, name: <t_sn>), // Novel

    // Level 1: Ys
    node((1, 3), $bold(y)_(11)$, name: <y_s1>),
    node((2, 3), $bold(y)_(21)$, name: <y_s2>),
    node((3, 3), $bold(y)_(31)$, name: <y_s3>),
    node((4, 3), $y_(41)^"new"$, name: <y_sn>),

    // Edges Shape
    edge(<hyp_s>, <t_s1>, "-|>"),
    edge(<hyp_s>, <t_s2>, "-|>"),
    edge(<hyp_s>, <t_s3>, "-|>"),
    edge(<hyp_s>, <t_sn>, "-|>"),
    edge(<t_s1>, <y_s1>, "-|>"),
    edge(<t_s2>, <y_s2>, "-|>"),
    edge(<t_s3>, <y_s3>, "-|>"),
    edge(<t_sn>, <y_sn>, "-|>"),

    // Visuals Shape (Homogeneous within category)
    node((1, 4), group-shapes("circle")),
    node((2, 4), group-shapes("rect")),
    node((3, 4), group-shapes("triangle")),
    node((4, 4), draw-shape("diamond", black)), // New: Diamond

    // ==========================================
    // --- DIMENSION 2: COLOR ---
    // ==========================================
    node((7.5, -2), [*Color Dimension* ($m=2$)], name: <dim_c>),

    node((7.5, -1), $lambda_2$, name: <hyperprior_c>),
    edge(<hyperprior_c>, <hyp_c>, "-|>"),

    // Level 3: Hyperparameters
    node((7.5, 0), $alpha_2, bold(beta)_2$, name: <hyp_c>),

    // Level 2: Thetas
    node((6, 1.5), $bold(theta)_(12)$, name: <t_c1>),
    node((7, 1.5), $bold(theta)_(22)$, name: <t_c2>),
    node((8, 1.5), $bold(theta)_(32)$, name: <t_c3>),
    node((9, 1.5), $bold(theta)_(42)$, name: <t_cn>),

    // Level 1: Ys
    node((6, 3), $bold(y)_(12)$, name: <y_c1>),
    node((7, 3), $bold(y)_(22)$, name: <y_c2>),
    node((8, 3), $bold(y)_(32)$, name: <y_c3>),
    node((9, 3), $y_(42)^"new"$, name: <y_cn>),

    // Edges Color
    edge(<hyp_c>, <t_c1>, "-|>"),
    edge(<hyp_c>, <t_c2>, "-|>"),
    edge(<hyp_c>, <t_c3>, "-|>"),
    edge(<hyp_c>, <t_cn>, "-|>"),
    edge(<t_c1>, <y_c1>, "-|>"),
    edge(<t_c2>, <y_c2>, "-|>"),
    edge(<t_c3>, <y_c3>, "-|>"),
    edge(<t_cn>, <y_cn>, "-|>"),

    // Visuals Color (High Variance within category)
    node((6, 4), group-shapes("circle")),
    node((7, 4), group-shapes("rect")),
    node((8, 4), group-shapes("triangle")),
    node((9, 4), draw-shape("diamond", black)), // New: Diamond


    // Divider Line
    edge((5, -1), (5, 4.5), stroke: (dash: "dashed", paint: gray)),

    // Labels for Columns
    node((2.5, 4.8), align(center)[*Low shape variance* \ Shape bias learned]),
    node((7.5, 4.8), align(center)[*High color variance* \ Color ignored]),
  )
]

