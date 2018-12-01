from .types import Func


def compile(tree):
    instrs = []
    if tree.type == "def":
        # TODO verify that not yet defined
        instrs.extend(compile(tree.value))
        instrs.append(("store", tree.symbol.value))
    elif tree.type == "set":
        # TODO verify that already defined
        instrs.extend((compile(tree.value)))
        instrs.append(("store", tree.symbol.value))
    elif tree.type == "cond":
        labels = ["c" + str(i) for i in range(len(tree.branches))]
        labels.extend(["else", "end"])
        offsets = dict((key, None) for key in labels)
        conds = []
        for (i, (pred, result)) in enumerate(tree.branches):
            offsets[labels[i]] = len(conds)
            conds.extend(compile(pred))
            conds.append(("ifn", labels[i+1]))
            conds.extend(compile(result))
            conds.append(("jump", "end"))
        offsets["else"] = len(conds)
        conds.extend(compile(tree.else_value))
        offsets["end"] = len(conds)
        cond_instrs = []
        for (i, (inst, arg)) in enumerate(conds):
            if inst in ["jump", "ifn"] and arg in offsets:
                cond_instrs.append((inst, offsets[arg] - i))
            else:
                cond_instrs.append((inst, arg))
        instrs.extend(cond_instrs)
    elif tree.type == "application":
        for a in tree.arguments:
            instrs.extend(compile(a))
        instrs.extend(compile(tree.operator))
        instrs.append(("apply", len(tree.arguments)))
    elif tree.type == "number":
        instrs.append(("push", tree.value))
    elif tree.type == "symbol":
        instrs.append(("read", tree.value))
    elif tree.type == "beagle":
        for f in tree.forms:
            instrs.extend(compile(f))
    elif tree.type == "fn":
        proc_instrs = []
        for f in tree.forms:
            proc_instrs.extend(compile(f))
        params = [p.value for p in tree.params]
        print "params?", tree.params, params
        instrs.append(("func", Func(params=params, instructions=proc_instrs)))
    return instrs
