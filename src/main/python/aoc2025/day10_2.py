import z3
import sys

file = open('day10.input', 'r')
lines = file.readlines()

i = 0
total = 0

while i < len(lines):
    data = lines[i].strip().split(" ")
    joltage = [int(x) for x in data[-1][1:-1].split(",")]
    buttons = [[int(x) for x in y[1:-1].split(",")] for y in data[1:-1]]

    presses = [z3.Int("p" + str(i)) for i in range(len(buttons))]

    buttonAdds = []
    for j in buttons:
        perButtonAdds = [0] * len(joltage)
        for k in j:
            perButtonAdds[k] = 1
        buttonAdds.append(perButtonAdds)

    outs = []
    for v in range(len(joltage)):
        buttonSum = z3.simplify(z3.Sum([p * b[v] for p,b in zip(presses, buttonAdds)]))
        outs.append(buttonSum)

    s = z3.Optimize()
    for j in range(len(presses)):
        s.add(*[presses[j] >= 0])
    for j in range(len(outs)):
        s.add(*[outs[j] == joltage[j]])

    s.minimize(z3.Sum(presses))

    s.check()
    model = s.model()
    result = sum([model[p].as_long() for p in presses])
    total = total + result

    i = i + 1

print(total)
