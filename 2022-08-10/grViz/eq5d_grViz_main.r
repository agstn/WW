# packages
pacman::p_load(DiagrammeR)
pacman::p_load(DiagrammeRsvg, rsvg)

# NCOND
ncond <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node   [fontsize = 10, fontname = Helvetica, fixedsize = true]
NCOND  [shape = ellipse, style = filled, fillcolor = '#FBB4AE', label = '#COND']
EQ5D   [shape = box, penwidth = 0.25, label = 'EQ-5D']

NCOND  -> EQ5D [ style = bold, arrowsize = 0.5]
}")

ncond  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_ncond.png")

# BMI
bmi <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node   [fontsize = 10, fontname = Helvetica, fixedsize = true]
BMI  [shape = ellipse, style = filled, fillcolor = '#B3CDE3']
EQ5D   [shape = box, penwidth = 0.25, label = 'EQ-5D']

BMI  -> EQ5D [ style = bold, arrowsize = 0.5]
}")

bmi  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_bmi.png")

# EDU
edu <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node   [fontsize = 10, fontname = Helvetica, fixedsize = true]
EDU  [shape = ellipse, style = filled, fillcolor = '#CCEBC5']
EQ5D   [shape = box, penwidth = 0.25, label = 'EQ-5D']

EDU  -> EQ5D [ style = bold, arrowsize = 0.5]
}")

edu  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_edu.png")

# SEX
sex <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node   [fontsize = 10, fontname = Helvetica, fixedsize = true]
SEX    [shape = ellipse, style = filled, fillcolor = '#DECBE4']
EQ5D   [shape = box, penwidth = 0.25, label = 'EQ-5D']

SEX  -> EQ5D [ style = bold, arrowsize = 0.5]
}")

sex  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_sex.png")

# MARSTA
marsta <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node   [fontsize = 10, fontname = Helvetica, fixedsize = true]
MARSTA [shape = ellipse, style = filled, fillcolor = '#FED9A6']
EQ5D   [shape = box, penwidth = 0.25, label = 'EQ-5D']

MARSTA  -> EQ5D [ style = bold, arrowsize = 0.5]
}")

marsta  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_marsta.png")

# AGE
age <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node   [fontsize = 10, fontname = Helvetica, fixedsize = true]
AGE    [shape = ellipse, style = filled, fillcolor = '#FFFFCC']
EQ5D   [shape = box, penwidth = 0.25, label = 'EQ-5D']

AGE  -> EQ5D [style = bold, arrowsize = 0.5]
}")

age  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_age.png")


