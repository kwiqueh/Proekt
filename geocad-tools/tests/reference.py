"""Independent Python reference, full precision; run with python3 reference.py."""
import json
from math import dist, fsum

RECTANGLE = [(0., 0.), (10., 0.), (10., 20.), (0., 20.)]
REVERSED = RECTANGLE[::-1]
DUPLICATE = [(0., 0.), (10., 0.), (10., 0.), (10., 20.), (0., 20.)]
# Fixture 6: normal WCS=(0,1,0), elevation=7; OCS axes=(-X,+Z).
# Altering UCS must not affect these WCS results.
TILTED_WCS = [(0., 7., 0.), (-10., 7., 0.), (-10., 7., 20.), (0., 7., 20.)]


def reference(points):
    """XY ring -> independent lengths/perimeter/signed area/duplicate pairs."""
    edges = list(zip(points, points[1:] + points[:1]))
    origin = points[0]
    area = fsum((a[0]-origin[0])*(b[1]-origin[1]) -
                (b[0]-origin[0])*(a[1]-origin[1]) for a, b in edges) / 2
    return dict(lengths=[dist(a, b) for a, b in edges],
                perimeter=fsum(dist(a, b) for a, b in edges),
                signed_area=area, area=abs(area),
                orientation='CCW' if area > 0 else 'CW' if area < 0 else 'DEGENERATE',
                duplicates=[(i+1, j+1) for i, a in enumerate(points)
                            for j, b in enumerate(points) if i < j and dist(a, b) <= .001])


if __name__ == '__main__':
    cases = {name: reference(points) for name, points in
             [('1_rectangle', RECTANGLE), ('2_reverse', REVERSED), ('3_duplicate', DUPLICATE),
              ('6_tilted_OCS', RECTANGLE)]}
    cases['3_duplicate']['accepted'] = False  # numeric values are diagnostic only
    cases['6_tilted_OCS']['WCS'] = TILTED_WCS
    print(json.dumps(cases, ensure_ascii=False, indent=2))
