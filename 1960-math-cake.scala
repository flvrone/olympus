//> using scala 3.7.1

package olimp.mathcake

import scala.annotation.tailrec

@main
def main(cakePieces: Int, people: Int): Unit =
  if cakePieces < people then println("not enough cake pieces")
  else if people == 1 || cakePieces == people then println("1")
  else
    val surplus = cakePieces - people
    val distribution = Array.fill(people)(1)
    distribution(people - 1) = surplus + 1

    val initDist = distribution.toVector
    println(countAllDists(initDist, None))

@tailrec
def countAllDists(
    initDist: Vector[Int],
    prevReceiverIndex: Option[Int],
    currentCount: Int = 1
): Int =
  println(initDist)
  nextDistribution(initDist, prevReceiverIndex) match
    case None => currentCount
    case Some((nextDist, ri)) =>
      countAllDists(nextDist, Some(ri), currentCount + 1)

def nextDistribution(
    dist: Vector[Int],
    prevReceiverIndex: Option[Int]
): Option[(Vector[Int], Int)] =
  val sum = dist.sum
  for (ri, di) <- findReceiverDonorIndices(dist)
  yield
    val receiver = dist(ri)
    val donor = dist(di)
    val newDist = dist.toArray
    newDist(ri) = receiver + 1
    newDist(di) = donor - 1

    val prevRi = prevReceiverIndex.getOrElse(ri)
    if ri < prevRi then
      println("rebalancing")
      // rebalance distribution - shift all extra diffs to the last element
      val baseLine = receiver + 1
      val lastIndex = newDist.length - 1

      for i <- (ri + 1) until lastIndex do
        val el = newDist(i)
        if el > baseLine then
          val diff = el - baseLine
          newDist(i) = baseLine
          newDist(lastIndex) += diff

    (newDist.toVector, ri)

def findReceiverDonorIndices(dist: Vector[Int]): Option[(Int, Int)] =
  var index = dist.length - 2
  findReceiverDonorIndices(dist, index)

@tailrec
def findReceiverDonorIndices(
    dist: Vector[Int],
    index: Int
): Option[(Int, Int)] =
  if index < 0 then None
  else if dist(index) < dist(index + 1) then
    findDonorIndex(dist, index) match
      case Some(donorIndex) => Some((index, donorIndex))
      case None             => findReceiverDonorIndices(dist, index - 1)
  else findReceiverDonorIndices(dist, index - 1)

def findDonorIndex(dist: Vector[Int], receiverIndex: Int): Option[Int] =
  val index = receiverIndex + 1
  findDonorIndex(dist, receiverIndex, index)

@tailrec
def findDonorIndex(
    dist: Vector[Int],
    receiverIndex: Int,
    index: Int
): Option[Int] =
  if index >= dist.length then None
  else
    val receiver = dist(receiverIndex)
    val donor = dist(index)
    if donor > receiver + 1 && donor > dist(index - 1) then Some(index)
    else findDonorIndex(dist, receiverIndex, index + 1)
