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

    s = z3.Solver()
    for j in range(len(presses)):
        s.add(*[presses[j] >= 0])
    for j in range(len(outs)):
        s.add(*[outs[j] == joltage[j]])

    found = True
    minscore = sys.maxsize
    while (found):
        found = s.check() == z3.sat
        if found:
            model = s.model()
            result = sum([model[p].as_long() for p in presses])
            if result < minscore:
                minscore = result
                print([model[p] for p in presses])
                s.add(z3.Sum([p for p in presses]) < result)

    if minscore < sys.maxsize:
        print(i, minscore)
        total = total + minscore

    i = i + 1

print("====")
print(total)
