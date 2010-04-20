var js = {
  x: 4,
  y0: function () {
    print("Me " + this.x);
  },
  y1: function (z) {
    print("Me " + this.x * z);
  }
};

var method = function (func) {
  if (!func.snowshoeMethod) {
      var wrapper = function () {
        var args = Array.prototype.slice.call(arguments);
        print("call " + arguments.length);
        func.apply(this, [this].concat(args));
      };
      wrapper.snowshoeMethod = true;
      return wrapper;
  }
  else {
      return func;
  }
};

var ss = {
  x: 3,
  y0: method(function (self) {
    print("Me " + self.x);
  }),
  y1: method(function (self, z) {
    print("Me " + self.x * z);
  })
};
var y1 = ss.y1;
