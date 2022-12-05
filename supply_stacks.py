#!/usr/bin/env python3

from copy import copy
import fileinput
from itertools import compress, cycle, islice, takewhile


def solve(stacks, operations, multiple_crates_func):
	for c, f, t in operations:
		g = stacks[f][:c]
		stacks[f] = stacks[f][c:]
		stacks[t] = multiple_crates_func(g) + stacks[t]

	return ''.join(s[0] for s in stacks)


def main():
	raw_data = fileinput.input()
	raw_data_it = iter(raw_data)
	raw_initial = takewhile(lambda s: not s[1].isdigit(), raw_data_it)
	stacks = [[*filter(' '.__ne__, x)] for x in zip(*(''.join(x) for x in map(lambda x: compress(x, cycle([0, 1, 0, 0])), raw_initial)))]
	raw_operations = islice(raw_data_it, 1, None)

	operations = [(int(x[1]), int(x[3]) - 1, int(x[5]) - 1) for x in (line.strip().split(' ') for line in raw_operations)]

	print(solve(copy(stacks), operations, lambda x: x[::-1]))
	print(solve(stacks, operations, lambda x: x))

if __name__ == '__main__':
	main()