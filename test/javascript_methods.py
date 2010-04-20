import types
from functools import wraps

class prototype(object):
  def func(self):
    self.func2()
  def func2(self):
    print "func2"

def add_method(obj, name, func):
  setattr(obj, name,
    types.MethodType(func, obj, obj.__class__))

py = prototype()
py.x = 4
def pyy0(self):
  print "Me " + str(self.x)
add_method(py, 'y0', pyy0)
def pyy1(self, z):
  print "Me " + str(self.x * z)
add_method(py, 'y1', pyy1)

snowshoeContext = prototype()
snowshoeContext.this = None

def ss_method(func):
  @wraps(func)
  def wrapper(self, *args, **kwds):
    old_this = snowshoeContext.this
    snowshoeContext.this = self
    func(*args, **kwds)
    snowshoeContext.this = old_this
  return wrapper

ss = prototype()
ss.x = 3
@ss_method
def ssy0():
  print "Me " + str(snowshoeContext.this.x)
add_method(ss, 'y0', ssy0)
@ss_method
def ssy1(z):
  print "Me " + str(snowshoeContext.this.x * z)
add_method(ss, 'y1', ssy1)

