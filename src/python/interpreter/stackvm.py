from .types import Frame, ClosureFrame, Closure, Op, Env
from . import runtime
import operator


bindings = {
    # TODO use custom types?  wrapped bools, ints?
    "true": True,
    "false": False,
    "car": runtime.list_car,
    "cdr": runtime.list_cdr,
    "+" : runtime.plus,
    "-" : runtime.minus,
    "*" : Op(operator.mul),
    "/" : Op(operator.div),
    "<" : Op(operator.lt),
    "<=": Op(operator.le),
    ">" : Op(operator.gt),
    ">=": Op(operator.ge),
    "==": Op(operator.eq)}
root_env = Env(bindings, "root")



def evaluate(instructions, env):
    closure_counter = 0

    stack = []
    code = [Frame("root", env, 0, instructions)]

    while len(code) > 0:
        frame = code[-1]
        # finished the current subroutine: pop its frame
        if frame.is_done():
            code.pop()
            continue
        # continue executing the current subroutine
        ins, arg = frame.next_inst()
        i = frame.i
        if ins == "read":
            print "read env?", list(frame.get_env().bindings.iterkeys()), type(frame.get_env().parent)
            print "name, parent name:", frame.get_env().name, frame.get_env().parent.name if frame.get_env().parent is not None else "<no parent>"
            print "frame:", frame.get_env()
            stack.append(frame.get_env().get(arg))
            i += 1
        elif ins == "ifn":
            val = stack.pop()
            if not val:
                i += arg
            else:
                i += 1
        elif ins == "jump":
            i += arg
        elif ins == "return":
            code.pop()
        elif ins == "push":
            stack.append(arg)
            i += 1
        elif ins == "store":
            frame.get_env().set(arg, stack.pop())
            i += 1
        elif ins == "func":
            func = arg
            print "func?", type(func), func
            stack.append(Closure(frame.get_env(), func, str(closure_counter)))
            closure_counter += 1
            i += 1
        elif ins == "apply":
            op = stack.pop()
            if op.is_builtin():
                args = []
                for _ in range(arg):
                    args.append(stack.pop())
                stack.append(op.apply(args[::-1]))
                i += 1
            else:
                bindings = {}
#                print "user-defined op:", dir(op), dir(op.__dict__), type(op.func), op.func
                for key in op.func.params[::-1]:
                    bindings[key] = stack.pop()
                code.append(ClosureFrame(0, bindings, op))
                i += 1
        elif ins == "print":
            print stack.pop()
            i += 1
        else:
            raise Exception("unrecognized instruction {}, arg {}".format(ins, arg))
        frame.i = i
    if len(stack) > 1:
        raise Exception("expected zero or one value on stack, found {} ({})".format(len(stack), stack))
    elif len(stack) == 0:
        return None
    ret_val = stack.pop()
    return ret_val
