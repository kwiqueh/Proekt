"""Mocked adapter contract tests; NOT verification of native CAD trans or API support."""
from dataclasses import dataclass
from pathlib import Path
from run_core import runtime, parse, sequence, evaluate, Symbol
from reference import RECTANGLE, TILTED_WCS


@dataclass
class Pair:
    code: int
    value: object


def main():
    env = runtime()
    output, transforms = [], []
    data = []

    def assoc(code, items):
        return next((p for p in items if p.code == code), None)

    def trans(point, source, target):
        assert source == 'entity' and target == 0
        transforms.append(point)
        return [-point[0], point[2], point[1]]

    env.update({'trans': trans, 'entget': lambda _: data,
                'assoc': assoc, 'logand': lambda a, b: a & b,
                'car': lambda x: x.code if isinstance(x, Pair) else x[0] if x else None,
                'cdr': lambda x: x.value if isinstance(x, Pair) else x[1:] if x else None,
                'cadr': lambda x: x.value[0] if isinstance(x, Pair) else x[1] if x and len(x)>1 else None,
                'caddr': lambda x: x.value[1] if isinstance(x, Pair) else x[2] if x and len(x) > 2 else None,
                '/=': lambda a, b: a != b,
                'equal': lambda a, b, tol: len(a) == len(b) and all(abs(x-y) <= tol for x,y in zip(a,b)),
                'rtos': lambda n, mode, digits: f'{n:.{digits}f}',
                'strcat': lambda *s: ''.join(s), 'itoa': str, 'fix': int,
                'princ': lambda *x: output.extend(x),
                'entsel': lambda _: ['entity', [0,0,0]]})
    root = Path(__file__).resolve().parents[1]
    sequence(parse((root/'src/gct-cad.lsp').read_text()), env)
    # Parse all native fixtures too; cannot run entmake in this mock.
    parse((root/'tests/fixtures.lsp').read_text())

    def fixture(flags=1, arc=0, normal=True):
        values = [Pair(0, 'LWPOLYLINE'), Pair(70, flags), Pair(90, 4), Pair(38, 7.)]
        if normal:
            values.append(Pair(210, [0.,1.,0.]))
        for i, p in enumerate(RECTANGLE):
            values.extend([Pair(10, list(p)), Pair(42, arc if i == 0 else 0.)])
        return values

    def command():
        output.clear()
        evaluate([Symbol('c:gct_info')], env)
        return ''.join(output)

    checks = 0
    data = fixture(flags=129)
    record = evaluate([Symbol('gct:read-polyline'), 'entity'], env)
    assert record[1] == [list(p) for p in TILTED_WCS]
    assert transforms == [[x,y,7.] for x,y in RECTANGLE]
    assert record[2] is True
    checks += 3
    text = command()
    assert '200.000' in text and '60.000' in text and 'Предупреждения: нет' in text
    checks += 1
    for flags, arc, message in [(0,0,'не замкнут'), (1,0.5,'контур содержит дуги')]:
        data = fixture(flags, arc)
        text = command()
        assert message in text and 'Периметр:' not in text and 'Площадь:' not in text
        checks += 1
    data = fixture()
    data[2] = Pair(90, 5)
    assert 'DXF 90' in command() and 'Периметр:' not in ''.join(output)
    checks += 1
    data = [Pair(0,'LINE')]
    assert 'Требуется LWPOLYLINE' in command()
    checks += 1
    env['entsel'] = lambda _: None
    assert 'отменён' in command()
    checks += 1
    print(f'PASS: {checks} adapter assertions with mocked CAD API; native trans remains untested.')


if __name__ == '__main__':
    main()
