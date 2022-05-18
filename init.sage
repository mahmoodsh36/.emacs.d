import re

# make sxiv the default image viewer
from sage.misc.viewer import viewer
viewer.png_viewer('sxiv')

# function to easily get power set
def pset(set):
  return Set(list(powerset(set)))

def latex_set_str(o):
  replaced_left = latex(o).replace('\\left[', '\\left\\{', 1)
  replaced_right = '\\right\\}'.join(replaced_left.rsplit('\\right]', 1))
  return ' '.join(replaced_right.splitlines())

def latex_set(o):
  print('$' + latex_set_str(o) + '$')

def latex_all_str(*a):
  return ' '.join(obj if type(obj) == str else latex_set_str(obj) if isinstance(obj, sage.sets.set.Set_object) else ' '.join(latex(obj).splitlines()) for obj in a)

def latex_all(*a):
  print('$' + latex_all_str(*a) + '$')

def latex_all_display(*a):
  print('\\[' + latex_all_str(*a) + '\\]')

# aliases to shorten code
l = latex_all
ld = latex_all_display

# square brackets look better
latex.matrix_delimiters("[", "]")

# enable ascii art by default
%display unicode_art

# functions to format a string with the latex of values of variables instead of the value itself
def handle(match):
  try:
    return latex_all_str(eval(match.group()[1:-1]))
  except:
    return match.group()
def my_format(mystr):
  print(re.sub(r"{.+?}", handle, mystr))
fm = my_format
