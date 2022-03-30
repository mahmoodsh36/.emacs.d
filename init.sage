# make sxiv the default image viewer
from sage.misc.viewer import viewer
viewer.png_viewer('sxiv')

# function to easily get power set
def pset(set):
  return Set(list(powerset(set)))

# enable ascii art by default
%display ascii_art
