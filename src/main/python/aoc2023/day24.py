import z3

hail = [[int(i) for i in l.replace('@',',').split(',')]
                for l in open('data.txt')]

rock = z3.RealVector('r', 6)
time = z3.RealVector('t', 3)

s = z3.Solver()
s.add(*[rock[d] + rock[d+3] * t == hail[d] + hail[d+3] * t
        for t, hail in zip(time, hail) for d in range(3)])
s.check()

print(s.model().eval(sum(rock[:3])))
