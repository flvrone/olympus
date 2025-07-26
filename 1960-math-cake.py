import sys
from itertools import repeat
from typing import Optional

def main(cake_pieces: int, people: int) -> None:
  if cake_pieces < people:
    print("not enough cake pieces")
  elif people == 1 or cake_pieces == people:
    print("1")
  else:
    surplus = cake_pieces - people
    distribution = list(repeat(1, people))
    distribution[people - 1] = surplus + 1

    print(countAllDists(distribution))


def countAllDists(
  dist: list[int], prev_receiver_index: int = 0, current_count: int = 1
) -> int:
  while True:
    print(dist)
    next_d = next_distribution(dist, prev_receiver_index)
    if next_d is None:
      return current_count
    else:
      dist, ri, di = next_d
      prev_receiver_index = ri

      current_count += 1

      ffd = try_fast_forward_distribution(dist, ri, di)
      if not ffd is None:
        dist, iterations = ffd
        current_count += iterations

def try_fast_forward_distribution(
  dist: list[int], r_index: int, d_index: int
) -> Optional[tuple[list[int], int]]:
  if r_index == d_index - 1:
    receiver = dist[r_index]
    donor = dist[d_index]
    diff = donor - receiver
    if diff > 1:
      print(dist)
      print("fast-forwarding...")

      iterations = diff // 2
      dist[r_index] = receiver + iterations
      dist[d_index] = donor - iterations

      print("calculated iterations:", iterations)

      return (dist, iterations)
    else:
      return None
  else:
    return None

def next_distribution(
  dist: list[int], prev_receiver_index: int = 0
) -> Optional[tuple[list[int], int, int]]:
  indices = find_receiver_donor_indices(dist, len(dist) - 2)
  if indices is None:
    return None
  else:
    ri, di = indices
    receiver = dist[ri]
    donor = dist[di]
    dist[ri] = receiver + 1
    dist[di] = donor - 1

    if ri < prev_receiver_index:
      print("rebalancing")
      # rebalance distribution - shift all extra diffs to the last element
      base_line = receiver + 1
      last_index = len(dist) - 1

      for i in range(ri + 1, last_index):
        el = dist[i]
        if el > base_line:
          diff = el - base_line
          dist[i] = base_line
          dist[last_index] += diff

    return (dist, ri, di)

def find_receiver_donor_indices(dist: list[int], r_index: int) -> Optional[tuple[int, int]]:
  if r_index < 0:
    return None
  elif dist[r_index] < dist[r_index + 1]:
    donor_index = find_donor_index(dist, r_index, r_index + 1)
    if not donor_index is None:
      return (r_index, donor_index)
    else:
      return find_receiver_donor_indices(dist, r_index - 1)
  else:
    return find_receiver_donor_indices(dist, r_index - 1)

def find_donor_index(dist: list[int], receiver_index: int, d_index: int) -> Optional[int]:
  if d_index >= len(dist):
   return None
  else:
    receiver = dist[receiver_index]
    donor = dist[d_index]
    beforeDonor = dist[d_index - 1]
    if donor > receiver + 1 and donor > beforeDonor:
      return d_index
    else:
      return find_donor_index(dist, receiver_index, d_index + 1)

if __name__ == "__main__":
  args = sys.argv
  if len(args) > 2:
    cake_pieces = int(args[1])
    people = int(args[2])
  else:
    cake_pieces = int(input())
    people = int(input())

  main(cake_pieces, people)
