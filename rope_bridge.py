#!/usr/bin/env python3

from copy import copy
from dataclasses import dataclass
import fileinput


@dataclass(unsafe_hash=True)
class Point:
	x: int
	y: int


def sgn(x):
	if x == 0:
		return 0
	return x / abs(x)


def main():
	movements = [(p[0], int(p[1])) for p in (line.strip().split(' ') for line in fileinput.input())]
	task1 = {Point(0, 0)}
	task2 = {Point(0, 0)}
	hpos = Point(0, 0)
	tpos = Point(0, 0)
	segments = [Point(0, 0) for _ in range(10)]

	for dir, n in movements:
		dx = (dir == 'R') - (dir == 'L')
		dy = (dir == 'U') - (dir == 'D')

		for _ in range(n):
			segments[0].x += dx
			segments[0].y += dy
			for i in range(1, 10):
				if max(abs(segments[i - 1].x - segments[i].x), abs(segments[i - 1].y - segments[i].y)) > 1:
					segments[i].x += sgn(segments[i - 1].x - segments[i].x)
					segments[i].y += sgn(segments[i - 1].y - segments[i].y)
			task1.add(copy(segments[1]))
			task2.add(copy(segments[-1]))

	print(len(task1))
	print(len(task2))


if __name__ == '__main__':
	main()