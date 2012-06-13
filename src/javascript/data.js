var Data = (function() {

function MyNumber(value) {
  this.value = value;
  this.type = 'number';
}


function MyString(value) {
  this.value = value;
  this.type = 'string';
}


function Symbol(value) {
  this.value = value;
  this.type = 'symbol';
}


function List(value) {
  this.value = value;
  this.type = 'list';
}


function MyFunction(value) {
  this.value = value;
  this.type = 'function';
}


function Nil() {
  this.value = false;
  this.type = 'nil';
}


return {
  'Number'         : function(x) {return new MyNumber(x)},
  'String'         : function(x) {return new MyString(x)},
  'Function'       : function(x) {return new MyFunction(x)},
  'List'           : function(x) {return new List(x)},
  'Symbol'         : function(x) {return new Symbol(x)},
  'Nil'            : function(x) {return new Nil(x)}
};
})();