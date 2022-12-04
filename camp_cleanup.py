from dataclasses import dataclass
import fileinput
import re


@dataclass
class Range:
	lbound: int
	ubound: int

	def contains(self, other):
		return self.lbound <= other.lbound and self.ubound >= other.ubound

	def overlaps(self, other):
		return self.lbound <= other.lbound and self.ubound >= other.lbound


line_regex = re.compile(r'^(\d+)-(\d+),(\d+)-(\d+)$')


def parse_line(line):
	match = line_regex.match(line)
	assert match is not None
	l1, u1, l2, u2 = map(int, match.groups())
	return Range(l1, u1), Range(l2, u2)


def main():
	pairs = [parse_line(line.strip()) for line in fileinput.input()]
	for method in ('contains', 'overlaps'):
		print(sum(map(lambda p: getattr(p[0], method)(p[1]) or getattr(p[1], method)(p[0]), pairs)))


if __name__ == '__main__':
	main()