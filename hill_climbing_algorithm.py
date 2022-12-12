#!/usr/bin/env python3

from dataclasses import dataclass
import fileinput
from queue import Queue


@dataclass
class Node:
	dist: int
	pos: tuple[int, int]


def bfs(board, start_pos, ending_nodes):
	q = Queue()
	visited = [[False for _ in range(len(board[0]))] for _ in range(len(board))]

	q.put(Node(0, start_pos))
	visited[start_pos[1]][start_pos[0]] = True

	while not q.empty():
		cn = q.get()
		x, y = cn.pos

		if board[y][x] in ending_nodes:
			return cn.dist

		for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
			nx, ny = x + dx, y + dy

			if nx < 0 or nx >= len(board[0]) or ny < 0 or ny >= len(board):
				continue

			f = ord(board[y][x]) if board[y][x] != 'E' else ord('z')
			t = ord(board[ny][nx]) if board[ny][nx] != 'S' else ord('a')

			if f - t > 1:
				continue

			if not visited[ny][nx]:
				q.put(Node(cn.dist + 1, (nx, ny)))
				visited[ny][nx] = True

	raise RuntimeError('Path not found (should never happen)')


def main():
	board = [line.strip() for line in fileinput.input()]
	start_pos = (0, 0)

	for y in range(len(board)):
		for x in range(len(board[0])):
			if board[y][x] == 'E':
				start_pos = (x, y)

	print(bfs(board, start_pos, ('S',)))
	print(bfs(board, start_pos, ('a', 'S')))


if __name__ == '__main__':
	main()