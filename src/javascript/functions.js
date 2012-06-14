var Functions = (function(Data) {

function cons(elem, list) {
  var newList = [elem];
  for(var i = 0; i < list.value.length; i++) {
    newList.push(list.value[i]);
  }
  return Data.List(newList);
};


function car(list) {
  if( list.value.length > 0 ) {
    return list.value[0];
  }
  
  return Data.Nil();
}


function cdr(list) {
  if( list.value.length == 0 ) {
    return Data.Nil();
  }
  
  return Data.List(list.value.slice(1));
}


function list() {
  var args = [];
  for(var i = 0; i < arguments.length; i++) {
    args.push(arguments[i]);
  }
  return Data.List(args);
}


return {
  'cons': cons,
  'car': car,
  'cdr': cdr,
  'list': list
};

})(Data);
