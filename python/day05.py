#!/usr/bin/env python3
import re

# input_file = "example05.txt"
# stack_count = 3

input_file = "input05.txt"
stack_count = 9

stacks = [[] for x in range(stack_count)]
instructions = []

with open(input_file) as f:
    [init, insts] = f.read().split("\n\n")
    for line in init.split("\n"):
        line = line.strip("\n")
        # print(line)

        for stack in range(stack_count):
            col = 4 * stack + 1
            if col < len(line) and line[col - 1] == "[" and line[col + 1] == "]":
                stacks[stack].append(line[col])

    for stack in range(stack_count):
        stacks[stack].reverse()

    for inst in insts.split("\n"):
        m = re.match("move (\d+) from (\d+) to (\d+)", inst)
        if m:
            instructions.append(m.groups())

# print(stacks)

def move9000(stacks, count, src, dst):
    for _ in range(count):
        crate = stacks[src - 1].pop()
        stacks[dst - 1].append(crate)
    pass

def move9001(stacks, count, src, dst):
    split = len(stacks[src - 1]) - count
    temp = stacks[src - 1][split:]
    stacks[src - 1] = stacks[src - 1][:split]
    stacks[dst - 1].extend(temp)

for inst in instructions:
    move9001(stacks, int(inst[0]), int(inst[1]), int(inst[2]))

# print(stacks)

result = ""
for stack in stacks:
    result = result + stack.pop()

print(result)
