pacman::p_load(DiagrammeR)
pacman::p_load(DiagrammeRsvg, rsvg)

all <- DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = LR]

node [fontsize = 10, fontname = Helvetica, fixedsize = true, arrowsize = 0.7]
AGE  [shape = ellipse, style = filled, fillcolor = '#FFFFCC']
BMI  [shape = ellipse, style = filled, fillcolor = '#B3CDE3']
EQ5D [shape = box, penwidth = 0.25, label = 'EQ-5D']

SEX    [shape = ellipse, style = filled, fillcolor = '#DECBE4']
MARSTA [shape = ellipse, style = filled, fillcolor = '#FED9A6']
EDU    [shape = ellipse, style = filled, fillcolor = '#CCEBC5']
NCOND  [shape = ellipse, style = filled, fillcolor = '#FBB4AE', label = '#COND']

NCOND      -> EQ5D [style = bold, arrowsize = 0.7]
EDU        -> EQ5D [style = bold, arrowsize = 0.7]

INTER1 [shape=point, width=0.05, height=0.05]
SEX           -> EQ5D   [style = bold, arrowsize = 0.7]
{SEX, MARSTA} -> INTER1 [style = bold,  dir=none, style = dashed]
INTER1        -> EQ5D   [style = bold, arrowsize = 0.7, style = dashed]
MARSTA        -> EQ5D   [style = bold, arrowsize = 0.7]

INTER2 [shape=point, width=0.05, height=0.05]
AGE          -> EQ5D   [style = bold, arrowsize = 0.7]
{AGE, BMI}   -> INTER2 [style = bold, dir=none, style = dashed]
INTER2       -> EQ5D   [style = bold, arrowsize = 0.7, style = dashed]
BMI          -> EQ5D   [style = bold, arrowsize = 0.7]

INTER3 [shape=point, width=0.05, height=0.05]
{AGE, MARSTA}-> INTER3 [style = bold, dir=none, style = dashed]
INTER3       -> EQ5D   [style = bold, arrowsize = 0.7, style = dashed]

INTER4 [shape=point, width=0.05, height=0.05]
{MARSTA, BMI}-> INTER4 [style = bold, dir=none, style = dashed]
INTER4       -> EQ5D   [style = bold, arrowsize = 0.7, style = dashed]
}")

all  %>% 
   export_svg %>% 
   charToRaw %>% 
   rsvg_png("grViz/_all.png")

