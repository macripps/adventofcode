import sys
lines = [x.rstrip() for x in sys.stdin.readlines()]
board = lines[:-2]
ops = lines[-1]

wrap = {}
def edge(face1, dir1, exit, face2, dir2, enter, rot):
  for k in range(50):
    p1 = (face1[0] + dir1[0] * k, face1[1] + dir1[1] * k)
    p2 = (face2[0] + dir2[0] * k, face2[1] + dir2[1] * k)
    wrap[(p1[0] + exit[0], p1[1] + exit[1])] = (p2, rot)
    wrap[(p2[0] + enter[0], p2[1] + enter[1])] = (p1, -rot)

# Front:
edge((0, 50), (1, 0), (0, -1), (149, 0), (-1, 0), (0, -1), 2) # Left
edge((0, 50), (0, 1), (-1, 0), (150, 0), (1, 0), (0, -1), 1) # Top

# Right:
edge((49, 100), (0, 1), (1, 0), (50, 99), (1, 0), (0, 1), 1) # Under
edge((0, 100), (0, 1), (-1, 0), (199, 0), (0, 1), (1, 0), 0) # Top
edge((0, 149), (1, 0), (0, 1), (149, 99), (-1, 0), (0, 1), 2) # Back

# Under:
edge((50, 50), (1, 0), (0, -1), (100, 0), (0, 1), (-1, 0), 3) # Left

# Back:
edge((149, 50), (0, 1), (1, 0), (150, 49), (1, 0), (0, 1), 1) # Top

dir = [ (0, 1), (1, 0), (0, -1), (-1, 0) ]
p,facing = (0, board[0].index('.')),0
i = step = 0
trace = [ list(r) for r in board ]
while i < len(ops):
  print(p, facing)
  step += 1
  if ops[i] == 'L':
    facing = (facing-1) % 4
    i += 1
  elif ops[i] == 'R':
    facing = (facing+1) % 4
    i += 1
  else:
    steps = ''
    while i < len(ops) and ops[i].isnumeric():
      steps += ops[i]
      i += 1
    for k in range(int(steps)):
      r1,c1 = p[0] + dir[facing][0], p[1] + dir[facing][1]
      f0 = facing

      if (r1,c1) in wrap:
        (r1,c1),ff = wrap[(r1,c1)]
        facing = (facing + ff) % 4

      if board[r1][c1] == '#':
        facing = f0
        break
      trace[p[0]][p[1]] = '>v<^'[f0]
      p = (r1,c1)
  if len(sys.argv) > 1 and step == int(sys.argv[1]): break
trace[p[0]][p[1]] = 'o'
# for b in trace:
#   print(''.join(b))
end = (p[0]+1, p[1]+1)
print(f'end pos = {end}')
print(f'answer = {end[0]*1000 + end[1]*4 + facing}')