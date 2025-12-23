input_ = """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124"""

import time

def search_invalid_ids(ranges, min_=2, max_=None):
	# Needed to ensure uniqueness
	ids = set() 

	for range_ in ranges:
		
		print(f"looking into range {range_[0]} to {range_[1]}")
		tried_once = False
		
		for n in range(len(str(range_[0])), len(str(range_[1])) + 1):
			print(f"searching for {n} digits candidates")

			# cannot find same number twice in single digit number
			if n < 2:
				continue

			for k in range(1, n // min_ + 1):
				print(f"-> {k}-digits sequence")
				if n % k:
					print(f"Can't fit length {k} in length {n}")
					continue

				if n // k > max_:
					print("tooo much repeat needed, skip")
					continue

				subn = int(str(range_[0])[:k]) if not tried_once else 10**(k-1)
				tried_once = True
				
				while (id_ := int(str(subn)*(n//k))) <= range_[1]:
					if range_[0] <= id_:
						print(f"id {id_} found")
						ids.add(id_)
					subn += 1

	return sum(ids)


def main1(ranges):
	return search_invalid_ids(ranges, 2, 2)


def main2(ranges):
	return search_invalid_ids(ranges, 2)


with open("Day02.txt") as f:
	ranges = [tuple(map(int, r))
		# for r in map(lambda x: x.split('-'), f.read().strip().split(','))] 
		for r in map(lambda x: x.split('-'), input_.split(','))] 

s = time.perf_counter()
x = main1(ranges)
assert x == 53420042388, x
e = time.perf_counter()
print(e - s)


s = time.perf_counter()
x = main2(ranges)
assert x == 69553832684, x
e = time.perf_counter()
print(e - s)



