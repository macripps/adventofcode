import z3
import sys

file = open('../../resources/aoc2024/day13.input', 'r')
lines = file.readlines()
i = 0
total = 0
while i < len(lines):
    lineA = lines[i].rstrip()
    lineB = lines[i+1].rstrip()
    linePrize = lines[i+2].rstrip()
    i = i + 4

    dA = lineA.split(": ")[1].split(", ")
    dB = lineB.split(": ")[1].split(", ")
    prize = linePrize.split(": ")[1].split(", ")

    dAX = int(dA[0][2:])
    dAY = int(dA[1][2:])
    dBX = int(dB[0][2:])
    dBY = int(dB[1][2:])
    prizeX = int(prize[0][2:]) + 10000000000000
    prizeY = int(prize[1][2:]) + 10000000000000

    pressesA = z3.Int('pressesA')
    pressesB = z3.Int('pressesB')

    s = z3.Solver()
    s.add(*[pressesA * dAX + pressesB * dBX == prizeX])
    s.add(*[pressesA * dAY + pressesB * dBY == prizeY])
    last_model = None
    found = True
    minScore = sys.maxsize
    while (found):
        found = (s.check() == z3.sat)
        if found:
            pressesAL = s.model()[pressesA].as_long()
            pressesBL = s.model()[pressesB].as_long()
            score = 3 * pressesAL + pressesBL
            if (score < minScore):
                minScore = score
                s.add(*[pressesA * 3 + pressesB < score])

    if (minScore < sys.maxsize):
        total = total + minScore

print(total)
