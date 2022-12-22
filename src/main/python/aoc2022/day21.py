import operator
 
OP = {'+':operator.add, '-':operator.sub, '*':operator.mul, '/':operator.floordiv}
 
def compute(monkeys, m):
    yell = monkeys[m]
    if yell is None or isinstance(yell, int):
        return yell
    
    m1, op, m2 = yell
    s1 = compute(monkeys, m1)
    s2 = compute(monkeys, m2)
    if s1 and s2:
        return OP[op](s1, s2)
    else:
        return None
 
def solve(monkeys, m, target):
    yell = monkeys[m]
    if isinstance(yell, int): return yell
    if yell is None: return target
    
    m1, op, m2 = yell
    s1 = compute(monkeys, m1)
    s2 = compute(monkeys, m2)
    
    if s1 is None: # x op s2 = target
        match op:
            case '+':
                return solve(monkeys, m1, target - s2)
            case '-':
                return solve(monkeys, m1, target + s2)
            case '*':
                return solve(monkeys, m1, target // s2)
            case '/':
                return solve(monkeys, m1, target * s2)
    
    elif s2 is None: # s1 op x = target
        match op:
            case '+':
                return solve(monkeys, m2, target - s1)
            case '-':
                return solve(monkeys, m2, -(target - s1))
            case '*':
                return solve(monkeys, m2, target // s1)
            case '/':
                return solve(monkeys, m2, s1 // target)
 
monkeys = {}
with open('day21.txt') as f:
    for line in f.readlines():
        line = line.strip().split(' ')
        key = line[0][:-1]
        if len(line) == 2:
            monkeys[key] = int(line[1])
        else:
            monkeys[key] = line[1:]
 
print('Part 1:', compute(monkeys, 'root'))
monkeys['humn'] = None
monkeys['root'][1] = '-'
print('Part 2:', solve(monkeys, 'root', 0))
