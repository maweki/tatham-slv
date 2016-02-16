import re
import sys
clauses = []

def add_clause(*v):
    clauses.append(' '.join(map(str,v)) + ' 0\n')

def sign(x):
    if x == 0:
        return 0
    return -1 if x < 0 else 1

def hits(fro, direction, to):
    dx, dy = direction
    if dx == 0:
        return to[0] == fro[0] and dy == sign(to[1] - fro[1])
    if dy == 0:
        return to[1] == fro[1] and dx == sign(to[0] - fro[0])
    _dx, _dy = to[0] - fro[0], to[1] - fro[1]
    return abs(_dx) == abs(_dy) and sign(_dx) == dx and sign(_dy) == dy

def parse(puzzle_str):
    directions = {
        'a': (0,-1), 'b': (1,-1), 'c': (1,0), 'd': (1,1),
        'e': (0,1),  'f': (-1,1), 'g': (-1,0),'h': (-1,-1)
    }

    size, _, definition = puzzle_str.partition(':')
    r_size = re.compile(r"^(?P<size_x>\d+)x(?P<size_y>\d+)$")
    r_defn = re.compile(r"(?P<defn>\d*[a-h])")
    size_t = tuple(map(int, r_size.match(size).groups()))
    w, h = size_t
    pos_to_dir= {}
    pos_to_hint={}
    for n, m in enumerate(r_defn.finditer(definition)):
        pos = (n % w, n // w)
        pos_to_dir[pos] = directions[m.group(0)[-1:]]
        hint = m.group(0)[:-1]
        if hint:
            pos_to_hint[pos] = int(hint)
    return w, h, pos_to_dir, pos_to_hint

def main():
    from itertools import product, combinations
    w, h, pos_to_dir, pos_to_hint = parse(sys.argv[1])
    vars = {(grid, pos):n + 1 for n, (grid, pos) in enumerate(product(pos_to_dir, range(1, w*h + 1)))}
    _vars = {v:k for k,v in vars.items()}

    # add hints
    for it in pos_to_hint.items():
        add_clause(vars[it])

    # constraints same position not twice
    for pos in pos_to_dir:
        for l, r in combinations(range(1, w*h + 1), 2):
            add_clause(-vars[(pos,l)], -vars[(pos,r)])
        # but at least some
        add_clause(*(vars[(pos, p)] for p in range(1, w*h + 1)))

    # constraints not two at same position
    for num in range(1, w*h + 1):
        for pos1, pos2 in combinations(pos_to_dir, 2):
            add_clause(-vars[(pos1,num)],-vars[(pos2,num)])
        # but at least some
        add_clause(*(vars[(p, num)] for p in pos_to_dir))

    # arrow-constraints
    for f, t in product(pos_to_dir, repeat=2):
        if not hits(f, pos_to_dir[f],t):
            for r in range(1, w*h):
                add_clause(-vars[(f,r)],-vars[(t,r+1)])


    clauses.append("p cnf %d %d\n" % (len(vars), len(clauses)))

    from tempfile import NamedTemporaryFile
    import subprocess
    with NamedTemporaryFile() as f, NamedTemporaryFile(mode="r") as target:
        while clauses:
            p = clauses.pop()
            f.write(p.encode('utf-8'))
        f.flush()
        subprocess.call(['minisat', f.name, target.name])
        result = target.read()
        trues = re.compile(r"(\n|^| )(\d+)")
        corr = dict(_vars[int(res.group(2))] for res in trues.finditer(result) if res.group(2) != "0")
        fill = max(len(str(v)) for v in corr.values())
        for l in range(h):
            for c in range(w):
                print(str(corr[(c,l)]).rjust(fill), end=" ")
            print("")





main()
