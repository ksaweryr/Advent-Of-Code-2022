#!/usr/bin/env python3

import fileinput
from textwrap import wrap


def window(lst, size):
	return [lst[i:i+size] for i in range(0, len(lst), size)]


def priority(c):
	base = ord(c) - ord('a' if c.islower() else 'A') + 1
	return base if c.islower() else base + 26


def find_common_item(*items):
	iters = [0 for _ in range(len(items))]

	try:
		while True:
			if all(map(lambda p: p[0][p[1]] == items[0][iters[0]], zip(items, iters))):
				return items[0][iters[0]]
			else:
				idx = min(enumerate(zip(items, iters)), key=lambda p: p[1][0][p[1][1]])[0]
				iters[idx] += 1
	except IndexError:
		raise RuntimeError('Couldn\'t find common item in:' + ''.join(f'\n\t{x}' for x in items))


def main():
	rucksacks = [line.strip() for line in fileinput.input()]
	print(sum(priority(find_common_item(*map(sorted, wrap(r, len(r) // 2)))) for r in rucksacks))
	print(sum(priority(find_common_item(*map(sorted, w))) for w in window(rucksacks, 3)))


if __name__ == '__main__':
	main()