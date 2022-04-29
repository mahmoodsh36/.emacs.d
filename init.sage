# make sxiv the default image viewer
from sage.misc.viewer import viewer
viewer.png_viewer('sxiv')

# function to easily get power set
def pset(set):
  return Set(list(powerset(set)))

def latex_set(o):
  print('$' + ' '.join(latex(o).replace('\\left[', '\\left\\{').replace('\\right]', '\\right\\}').splitlines()) + '$')

def latex_all(o):
  print('$' + ' '.join(latex(o).splitlines()) + '$')

# enable ascii art by default
%display unicode_art
