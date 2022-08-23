# packages
pacman::p_load(DiagrammeR)
pacman::p_load(DiagrammeRsvg, rsvg)

# SEX & MARSTA
sex_marsta <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR, nodesep = 0.05]

node    [fontsize = 10, fontname = Helvetica, fixedsize = true]
SEX     [shape = ellipse, style = filled, fillcolor = '#DECBE4']
MARSTA  [shape = ellipse, style = filled, fillcolor = '#FED9A6']
EQ5D    [shape = box, penwidth = 0.25, label = 'EQ-5D']
INTER   [shape=point, width=0.05, height=0.05]

{SEX, MARSTA} -> INTER [style = bold, dir=none,        style = dashed]
INTER         -> EQ5D [ style = bold, arrowsize = 0.5, style = dashed]

}")

sex_marsta %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_sex_marsta.png")

# AGE & MARSTA
age_marsta <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR, nodesep = 0.05]

node [fontsize = 10, fontname = Helvetica, fixedsize = true]
MARSTA  [shape = ellipse, style = filled, fillcolor = '#FED9A6']
AGE  [shape = ellipse, style = filled, fillcolor = '#FFFFCC']
EQ5D [shape = box, penwidth = 0.25, label = 'EQ-5D']

INTER [shape=point, width=0.05, height=0.05]

{MARSTA, AGE} -> INTER [style = bold, dir=none,        style = dashed]
INTER         -> EQ5D [ style = bold, arrowsize = 0.5, style = dashed]

}")

age_marsta %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_age_marsta.png")

# MARSTA & BMI
marsta_bmi <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR, nodesep = 0.05]

node [fontsize = 10, fontname = Helvetica, fixedsize = true]
BMI  [shape = ellipse, style = filled, fillcolor = '#B3CDE3']
MARSTA  [shape = ellipse, style = filled, fillcolor = '#FED9A6']
EQ5D [shape = box, penwidth = 0.25, label = 'EQ-5D']

INTER [shape=point, width=0.05, height=0.05]

{BMI, MARSTA} -> INTER [style = bold, dir=none,        style = dashed]
INTER         -> EQ5D [ style = bold, arrowsize = 0.5, style = dashed]

}")

marsta_bmi %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_marsta_bmi.png")

# AGE & BMI
age_bmi <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR, nodesep = 0.05]

node [fontsize = 10, fontname = Helvetica, fixedsize = true]
BMI  [shape = ellipse, style = filled, fillcolor = '#B3CDE3']
AGE  [shape = ellipse, style = filled, fillcolor = '#FFFFCC']
EQ5D [shape = box, penwidth = 0.25, label = 'EQ-5D']

INTER [shape=point, width=0.05, height=0.05]

{BMI, AGE} -> INTER [style = bold, dir=none,        style = dashed]
INTER      -> EQ5D [ style = bold, arrowsize = 0.5, style = dashed]

}")

age_bmi %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_age_bmi.png")
