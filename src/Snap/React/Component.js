"use strict";

var React = require("react");

function Input(props) {
  React.Component.constructor.call(this);
  var self = this;

  self.set = function(v) {
    self.setState(
      function() {
        return v;
      },
      function() {
        props.set(v)();
      }
    );
  };

  self.state = props.s;
}

Input.prototype = Object.create(React.Component.prototype);

function enforceFocus() {
  var self = this;

  if (self.node) {
    if (self.state.focused) {
      self.node.focus();
    } else {
      self.node.blur();
    }
  }
}

Input.prototype.componentDidMount = enforceFocus;
Input.prototype.componentDidUpdate = enforceFocus;

Input.prototype.render = function() {
  var self = this;
  return React.createElement("input", {
    ref: function(r) {
      self.node = r;
    },
    value: self.state.value,
    onChange: function(e) {
      var v = e.target.value;
      self.set(Object.assign({}, self.state, { value: v }));
    },
    onFocus: function() {
      if (!self.state.focused) {
        self.set(Object.assign({}, self.state, { focused: true }));
      }
    },
    onBlur: function() {
      if (self.state.focused) {
        self.set(Object.assign({}, self.state, { focused: false }));
      }
    }
  });
};

var focusedInputComponent = function(set) {
  return function(s) {
    return React.createElement(Input, { set: set, s: s });
  };
};

exports.focusedInputComponent = focusedInputComponent;
