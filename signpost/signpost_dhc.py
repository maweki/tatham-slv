import re
import sys

class Node(object):
    hint = None
    following = frozenset()

    def __init__(self, direction):
        self.direction = direction

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
    nodes = {}
    for n, m in enumerate(r_defn.finditer(definition)):
        pos = (n % w, n // w)
        nodes[pos] = Node(directions[m.group(0)[-1:]])
        hint = m.group(0)[:-1]
        if hint:
            nodes[pos].hint = int(hint)
    return w, h, nodes

def solve_dhc(nodes):
    first = next(iter(n for n in nodes.values() if n.hint == 1))
    hints = frozenset(n.hint for n in nodes.values() if n.hint is not None)
    from collections import deque
    q = deque([[first]])
    while q:
        curr = q.pop()
        idx = len(curr)
        if (idx in hints or curr[-1].hint is not None) and curr[-1].hint != idx:
            continue
        if idx == len(nodes):
            return curr
        for _next in curr[-1].following:
            if _next not in curr:
                q.append(curr + [_next])

def main():
    w, h, nodes = parse(sys.argv[1])
    from itertools import product
    for x, y in product(range(w), range(h)):
        this_node = nodes[(x,y)]
        dx, dy = this_node.direction
        x, y = x + dx, y + dy
        while x >= 0 and x < w and y >= 0 and y < h:
            this_node.following = this_node.following | frozenset([nodes[x,y]])
            x, y = x + dx, y + dy

    dhc = solve_dhc(nodes)

    fill = len(str(w*h))
    for l in range(h):
        for c in range(w):
            node = nodes[(c,l)]
            print(str(dhc.index(node) + 1).rjust(fill), end=" ")
        print("")

main()
