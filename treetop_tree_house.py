#!/usr/bin/env python3

import fileinput
from itertools import accumulate


def transpose(grid):
	return [[grid[x][y] for x in range(len(grid[0]))] for y in range(len(grid))]


def max_heights(row):
	return [*accumulate(row, max)]


def task1(grid):
	transposed_grid = transpose(grid)
	left = [max_heights(row) for row in grid]
	right = [max_heights(reversed(row)) for row in grid]
	top = [max_heights(row) for row in transposed_grid]
	bottom = [max_heights(reversed(row)) for row in transposed_grid]

	res = (len(grid) + len(grid[0])) * 2 - 4 # set to border size initially

	for y in range(1, len(grid) - 1):
		for x in range(1, len(grid[0]) - 1):
			tree = grid[y][x]
			res += any(map(tree.__gt__, (left[y][x - 1], right[y][-x - 2], top[x][y - 1], bottom[x][-y - 2])))
	
	return res


def task2(grid):
	max_score = 0

	for y in range(len(grid)):
		for x in range(len(grid[0])):
			tree = grid[y][x]
			score = 1

			for dy in range(1, len(grid) - y):
				if grid[y + dy][x] >= tree:
					score *= dy
					break
			else:
				score *= len(grid) - y - 1

			for dy in range(1, y + 1):
				if grid[y - dy][x] >= tree:
					score *= dy
					break
			else:
				score *= y

			for dx in range(1, len(grid[0]) - x):
				if grid[y][x + dx] >= tree:
					score *= dx
					break
			else:
				score *= len(grid[0]) - x - 1

			for dx in range(1, x + 1):
				if grid[y][x - dx] >= tree:
					score *= dx
					break
			else:
				score *= x

			max_score = max(max_score, score)

	return max_score


def main():
	grid = [[int(x) for x in row.strip()] for row in fileinput.input()]
	print(task1(grid))
	print(task2(grid))


if __name__ == '__main__':
	main()