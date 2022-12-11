#!/usr/bin/env python3

from copy import copy
import fileinput
from functools import reduce
from math import lcm


class Simulation:
	def __init__(self, monkeys, rounds, safe_worry_levels):
		self.monkeys = monkeys
		self.rounds = rounds
		self.safe_worry_levels = safe_worry_levels
		self.mod = reduce(lcm, (m.test_value for m in monkeys))

	def run(self):
		for _ in range(self.rounds):
			for m in self.monkeys:
				m.inspect_items(self)

		scores = [m.inspected_count for m in self.monkeys]
		return int.__mul__(*sorted(scores, key=lambda x: -x)[:2])

	def throw_item(self, to, item):
		self.monkeys[to].items.append(item)


class Monkey:
	def __init__(self, items, operation, next_monkey, test_value):
		self.items = items
		self.operation = operation
		self.next_monkey = next_monkey
		self.test_value = test_value
		self.inspected_count = 0

	@classmethod
	def from_description(cls, desc):
		[_, items_line, operation_line, test_line, true_line, false_line] = desc.split('\n')
		items = [int(x) for x in items_line.split(': ')[1].split(', ')]
		op_repr, val = operation_line.split(' ')[-2:]
		if val == 'old':
			f = {'+': int.__add__, '*': int.__mul__}[op_repr]
			operation = lambda x: f(x, x)
		else:
			operation = {'+': int(val).__add__, '*': int(val).__mul__}[op_repr]
		test_value = int(test_line.split(' ')[-1])
		if_true = int(true_line.split(' ')[-1])
		if_false = int(false_line.split(' ')[-1])
		next_monkey = lambda x: if_true if x % test_value == 0 else if_false
		return cls(items, operation, next_monkey, test_value)

	def inspect_items(self, sim):
		for item in self.items:
			self.inspected_count += 1
			worry_level = self.operation(item) % sim.mod
			if sim.safe_worry_levels:
				worry_level //= 3
			sim.throw_item(self.next_monkey(worry_level), worry_level)

		self.items.clear()

	def __copy__(self):
		return self.__class__(copy(self.items), self.operation, self.next_monkey, self.test_value)


def main():
	data = ''.join(fileinput.input()).strip().split('\n\n')
	monkeys = [Monkey.from_description(desc) for desc in data]

	for rounds, safe_worry_levels in ((20, True), (10000, False)):
		sim = Simulation([copy(m) for m in monkeys], rounds, safe_worry_levels)
		print(sim.run())


if __name__ == '__main__':
	main()