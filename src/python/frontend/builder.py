import ast
from unparse.maybeerror import MaybeError as M


def number(cst):
    text = ''.join(cst['int'])
    return M.pure(ast.Number(int(text)))

ESCAPES = {'"': '"', '\\': '\\'}

def string(cst):
    chars = list(ESCAPES[c['char']] if c['_name'] == 'escape' else c['char'] for c in cst['chars'])
    return M.pure(ast.String(chars))

def symbol(cst):
    name = cst['first'] + ''.join(cst['rest'])
    return M.pure(ast.Symbol(name))

def bg_list(cst):
    elems = []
    for form in cst['body']:
        ast_err = build_ast(form)
        if ast_err.status != 'success':
            return ast_err
        elems.append(ast_err.value)
    return M.pure(ast.List(elems))

def app(cst):
    op_err = build_ast(cst['operator'])
    # TODO check whether `op` is a fn or symbol
    if op_err.status != 'success':
        return op_err
    args = []
    for a in cst['args']:
        arg_err = build_ast(a)
        if arg_err.status != 'success':
            return arg_err
        args.append(arg_err.value)
    return M.pure(ast.Application(op_err.value, args))

def bg_def(cst):
    symbol_err = build_ast(cst['symbol'])
    form_err = build_ast(cst['form'])
    if symbol_err.status != 'success':
        return symbol_err
    if form_err.status != 'success':
        return form_err
    return M.pure(ast.Def(symbol_err.value, form_err.value))

def bg_set(cst):
    symbol_err = build_ast(cst['symbol'])
    form_err = build_ast(cst['form'])
    if symbol_err.status != 'success':
        return symbol_err
    if form_err.status != 'success':
        return form_err
    return M.pure(ast.Set(symbol_err.value, form_err.value))

def fn(cst):
#    console.log('fn -- ' + Object.getOwnPropertyNames(cst))
    params = []
    params_set = set()
    for cst_param in cst['parameters']:
        param_err = build_ast(cst_param) # TODO expecting this to be a symbol
        if param_err.status != 'success':
            return param_err
#        console.log('fn symbol: ' + JSON.stringify(param))
        param = param_err.value
        if param.value in params_set:
            # TODO -- positions
            return M.error({'message': 'duplicate parameter name', 'name': param.name, 'positions': None})
        params.append(param)
        params_set.add(param.value)
    forms = []
    for form in cst['forms']:
        form_err = build_ast(form)
        if form_err.status != 'success':
            return form_err
        forms.append(form_err.value)
    return M.pure(ast.Fn(params, forms))

def cond(cst):
    branches = []
#    console.log('cond -- ' + JSON.stringify(cst, null, 2))
    for pair in cst['pairs']:
#        console.log('cond pair -- ' + JSON.stringify(cst.pairs[i]))
        pred = build_ast(pair['condition'])
        result = build_ast(pair['result'])
        if pred.status != 'success':
            return pred
        if result.status != 'success':
            return result
        branches.append([pred.value, result.value])
    else_form = build_ast(cst['else'])
    if else_form.status != 'success':
        return else_form
    return M.pure(ast.Cond(branches, else_form.value))

def beagle(cst):
    forms = []
    for cst_node in cst['forms']:
        ast_err = build_ast(cst_node)
        if ast_err.status != 'success':
            return ast_err
        forms.append(ast_err.value)
    return M.pure(ast.Beagle(forms))

SPECIALS = {
        'cond'  : cond  ,
        'set'   : bg_set,
        'def'   : bg_def,
        'fn'    : fn    ,
    }

def special(node):
    val = node['value']
    return SPECIALS[val['_name']](val)

NODES = {
        'number' : number ,
        'string' : string ,
        'symbol' : symbol ,
        'list'   : bg_list,
        'app'    : app    ,
        'special': special,
        'beagle' : beagle ,
    }

def build_ast(node):
#    print "node:", node
    if node['_name'] in NODES:
        ast = NODES[node['_name']](node)
#        if (false) { // uncomment to get rid of debugging information
        # TODO: it looks like unparse needs to be updated to support this feature,
        #   because currently it might be behind unparse-js
#        if ast.status == 'success':
#            ast.value._debug = {
#                'start': node['_start'],
#                'end': node['_end']
#            }
        return ast
#    console.log('invalid node name: ' + util.inspect(node, {'depth': null}))
    raise Exception('invalid node name: ' + node._name)
