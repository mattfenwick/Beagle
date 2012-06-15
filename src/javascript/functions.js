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


function equals(left, right) {
	var ltype = left.type,
	    rtype = right.type,
	    lval = left.value,
	    rval = right.value,
	    i;
	
	if( ltype !== rtype ) {
		return Data.Nil();
	}
	
	if( ltype === 'function' || ltype === 'specialform' || ltype === 'nil' ) {
		return Data.Nil();
	}
	
	// should a 'nil' in a list cause the comparison to return a 'nil'?
	// for now, we'll say ... no
	if( ltype === 'list' ) {
		if( lval.length !== rval.length ) {
			return Data.Boolean(false);
		}
		
		for(i = 0; i < lval.length; i++) {
			if( lval[i].type !== rval[i].type ) {
				return Data.Boolean(false);
			}
			if( !equals(lval[i], rval[i]) ) {
				return Data.Boolean(false);
			};
		}
		return Data.Boolean(true);
	}
	
	if( ltype === 'number' || ltype === 'string' || ltype === 'symbol' || ltype === 'boolean' ) {
		return Data.Boolean(lval === rval);
	}
	
	throw new Error("unrecognized type: " + ltype);
}


return {
  'cons': cons,
  'car': car,
  'cdr': cdr,
  'list': list,
  '=': equals
};

})(Data);
