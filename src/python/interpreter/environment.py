class Env(object):
    def __init__(self, name, bindings, parent=None):
        self.name = name
        self.bindings = bindings
        self.parent = parent

    def get_own_binding(self, key):
        return self.bindings.get(key, None)
    
    def get_binding(self, key):
        """ TODO make this iterative """
        val = self.get_own_binding(key)
        if val is not None:
            return val
        if self.parent is None:
            return None
        return self.parent.get_binding(key)
    
    def has_own_binding(self, key):
        return self.get_own_binding(key) is not None
    
    def has_binding(self, key):
        return self.has_binding(key) is not None
    
    def add_binding(self, key, value):
        if key in self.bindings:
            raise Exception("environment {} already has binding for {} ({})".format(self.name, key, self.bindings[key]))
        self.bindings[key] = value
    
    def set_binding(self, key, value):
        if key in self.bindings:
            self.bindings[key] = value
        elif self.parent is not None:
            self.parent.set_binding(key, value)
        else:
            raise Exception("environment {} does not have binding for {}, cannot set".format(self.name, key))
