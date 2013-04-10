

############################################

preamble = '''
define(function() {

    return ['''
    
close = '''].join("\\n");

});
'''

def escapeChar(c):
    if c == "'":
        return "\\'"
    return c

def escapeJS(str):
    return ''.join(map(escapeChar, str))

def formatJS(str):
    # escape all single quotes
    escaped = escapeJS(str)
    # what about \r or \f newlines?
    lines = escaped.split('\n')
    return ', '.join(["'%s'" % l for l in lines])


example = '''
define(function() {
    // auto-generated at <time/date>

    return ['abc', 'def', 'ghi', ...].join('\n');

});
'''


def beagleToJS(str):
    return preamble + formatJS(str) + close
    
############################################

files = ['core', 'dict', 'equality', 'generic', 'math', 'simple-table']


if __name__ == "__main__":
    for file in files:
        try:
            with open('beagle/' + file + '.bgl', 'r') as f:
                with open('javascript/beagle/' + file + '.js', 'w') as out:
                    out.write(beagleToJS(f.read()))
        except:
            print "error with file " + file
            raise
