class Evaluator(object):
    def __init__(self):
        self.code = []
        self.args = []
        self.ops = []
        self.op_table = {
            'eval': self.eval,
            
        }
    
    def evaluate(self, code):
        self.code.append(code)
        self.ops.append(('eval', 1))
        while len(self.ops) > 0:
