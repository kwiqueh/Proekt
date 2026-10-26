"""Execute actual core source in a deliberately small AutoLISP subset interpreter.
Not a CAD emulator: trans/DXF/command behavior needs the native manual tests.
No third-party dependencies. Run: python3 tests/run_core.py
"""
import math
import operator
import re
from pathlib import Path
from reference import RECTANGLE, REVERSED, DUPLICATE, reference


class Symbol(str):
    pass


def parse(source):
    tokens = iter(re.findall(r';[^\n]*|"(?:\\.|[^"\\])*"|[()\x27]|[^\s()\x27]+', source))
    tokens = iter(t for t in tokens if not t.startswith(';'))

    def read(t):
        if t == '(':
            out = []
            for t in tokens:
                if t == ')':
                    return out
                out.append(read(t))
            raise ValueError('Unclosed form')
        if t == "'":
            return [Symbol('quote'), read(next(tokens))]
        if t.startswith('"'):
            return t[1:-1]
        try:
            return float(t) if any(c in t for c in '.eE') else int(t)
        except ValueError:
            return Symbol(t.lower())
    return [read(t) for t in tokens]


class Env(dict):
    def __init__(self, parent=None):
        super().__init__()
        self.parent = parent

    def getvar(self, key):
        return self[key] if key in self else self.parent.getvar(key) if self.parent else None

    def put(self, key, value):
        scope = self
        while key not in scope and scope.parent:
            scope = scope.parent
        scope[key] = value
        return value


def truth(x):
    return x is not None and x is not False and x != []


def evaluate(x, env):
    if isinstance(x, Symbol):
        return env.getvar(x)
    if not isinstance(x, list):
        return x
    if not x:
        return None
    op, *args = x
    if op == 'quote':
        return args[0]
    if op == 'defun':
        env[args[0]] = ('function', args[1], args[2:])
        return args[0]
    if op == 'setq':
        result = None
        for key, val in zip(args[::2], args[1::2]):
            result = env.put(key, evaluate(val, env))
        return result
    if op == 'if':
        return evaluate(args[1], env) if truth(evaluate(args[0], env)) else evaluate(args[2], env) if len(args) > 2 else None
    if op in ('and', 'or'):
        for arg in args:
            result = evaluate(arg, env)
            if truth(result) == (op == 'or'):
                return result
        return True if op == 'and' else None
    if op == 'cond':
        for clause in args:
            if truth(evaluate(clause[0], env)):
                return sequence(clause[1:], env)
        return None
    if op == 'progn':
        return sequence(args, env)
    if op == 'while':
        while truth(evaluate(args[0], env)):
            sequence(args[1:], env)
        return None
    if op == 'foreach':
        for item in evaluate(args[1], env) or []:
            env.put(args[0], item)
            sequence(args[2:], env)
        return None
    fn = env.getvar(op)
    values = [evaluate(a, env) for a in args]
    if callable(fn):
        return fn(*values)
    if not fn:
        raise ValueError(f'Unsupported function: {op}')
    _, params, body = fn
    scope = Env(env)
    split = params.index('/') if '/' in params else len(params)
    scope.update(zip(params[:split], values))
    scope.update({key: None for key in params[split+1:]})
    return sequence(body, scope)


def sequence(forms, env):
    value = None
    for form in forms:
        value = evaluate(form, env)
    return value


def runtime():
    env = Env()
    env.update({'t': True, 'nil': None, '+': lambda *x: sum(x),
                '-': lambda a, *b: a-sum(b) if b else -a,
                '*': lambda *x: math.prod(x), '/': operator.truediv,
                'sqrt': math.sqrt, 'abs': abs, 'min': min, 'max': max,
                'car': lambda x: x[0] if x else None,
                'cdr': lambda x: x[1:] if x else None,
                'cadr': lambda x: x[1] if x and len(x)>1 else None,
                'cons': lambda a, b: [a] + (b or []), 'list': lambda *x: list(x),
                'append': lambda *x: sum((a or [] for a in x), []),
                'reverse': lambda x: list(reversed(x or [])),
                'length': lambda x: len(x or []), 'nth': lambda i, x: x[i],
                'listp': lambda x: x is None or isinstance(x, (list, tuple)),
                'numberp': lambda x: isinstance(x, (int, float)) and not isinstance(x, bool),
                'not': lambda x: not truth(x), 'princ': lambda *x: None})
    for name, cmp in [('=', operator.eq), ('<', operator.lt), ('>', operator.gt),
                      ('<=', operator.le), ('>=', operator.ge)]:
        env[name] = lambda *x, cmp=cmp: all(cmp(a, b) for a, b in zip(x, x[1:]))
    source = Path(__file__).resolve().parents[1] / 'src/gct-core.lsp'
    sequence(parse(source.read_text()), env)
    return env


def main():
    env = runtime()
    def call(name, *values):
        return evaluate([Symbol('gct:'+name)] + [[Symbol('quote'), x] for x in values], env)
    checks = 0
    for points in [RECTANGLE, REVERSED, DUPLICATE,
                   [(x+1e9, y+1e9) for x, y in RECTANGLE]]:
        expected = reference(points)
        for name, value in [('area-gauss', expected['signed_area']), ('perimeter', expected['perimeter'])]:
            assert math.isclose(call(name, points), value, abs_tol=1e-10)
            checks += 1
        assert call('segment-lengths', points) == expected['lengths']
        assert call('orientation', points) == expected['orientation']
        assert (call('find-duplicates', points, .001) or []) == [list(p) for p in expected['duplicates']]
        checks += 3
    cases = [
        (RECTANGLE, True, False, False), (REVERSED, True, False, False),
        (DUPLICATE, True, False, True), (RECTANGLE, False, False, True),
        ([(0,0),(10,20),(0,20),(10,0)], True, False, True),
        (RECTANGLE, True, True, True), ([(0,0),(1,0)], True, False, True),
        ([(0,0),(10,0),(5,0),(5,10)], True, False, True),
        ([(0,0),(10,0),(10,10),(5,0),(0,10)], True, False, True),
        ([(0,0),(.0005,0),(10,20)], True, False, True),
        ([(0,0),(10,0),(20,0)], True, False, True)]
    for pts, closed, arcs, bad in cases:
        assert truth(call('validate', pts, closed, arcs, .001)) == bad, pts
        checks += 1
    for pts, tol in [(RECTANGLE, 0), (RECTANGLE, -1), (['bad'], .001), ([(1e14,0),(1e14+10,0),(1e14,20)], .001)]:
        assert truth(call('validate', pts, True, False, tol))
        checks += 1
    print(f'PASS: {checks} core assertions executed against actual gct-core.lsp (subset interpreter).')


if __name__ == '__main__':
    main()
