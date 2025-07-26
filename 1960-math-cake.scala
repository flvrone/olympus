//> using scala 3.7.1

package olympus.mathcake

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
    case Some((newDist, ri, di)) =>
      tryFastForwardDistribution(newDist, ri, di) match
        case None => countAllDists(newDist, Some(ri), currentCount + 1)
        case Some((futureDist, iterations)) =>
          countAllDists(futureDist, Some(ri), currentCount + 1 + iterations)

def tryFastForwardDistribution(
    dist: Vector[Int],
    rIndex: Int,
    dIndex: Int
): Option[(Vector[Int], Int)] =
  if rIndex == dIndex - 1 then
    val receiver = dist(rIndex)
    val donor = dist(dIndex)
    val diff = donor - receiver
    if diff > 1 then
      val iterations: Int = diff / 2
      val futureDist = dist.toArray
      futureDist(rIndex) = receiver + iterations
      futureDist(dIndex) = donor - iterations

      println(dist)
      println("fast-forwarding...")
      println(s"calculated iterations: $iterations")

      Some((futureDist.toVector, iterations))
    else None
  else None

def nextDistribution(
    dist: Vector[Int],
    prevReceiverIndex: Option[Int]
): Option[(Vector[Int], Int, Int)] =
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

    (newDist.toVector, ri, di)

def findReceiverDonorIndices(dist: Vector[Int]): Option[(Int, Int)] =
  var index = dist.length - 2
  findReceiverDonorIndices(dist, index)

@tailrec
def findReceiverDonorIndices(
    dist: Vector[Int],
    rIndex: Int
): Option[(Int, Int)] =
  if rIndex < 0 then None
  else if dist(rIndex) < dist(rIndex + 1) then
    findDonorIndex(dist, rIndex) match
      case Some(donorIndex) => Some((rIndex, donorIndex))
      case None             => findReceiverDonorIndices(dist, rIndex - 1)
  else findReceiverDonorIndices(dist, rIndex - 1)

def findDonorIndex(dist: Vector[Int], receiverIndex: Int): Option[Int] =
  val dIndex = receiverIndex + 1
  findDonorIndex(dist, receiverIndex, dIndex)

@tailrec
def findDonorIndex(
    dist: Vector[Int],
    receiverIndex: Int,
    dIndex: Int
): Option[Int] =
  if dIndex >= dist.length then None
  else
    val receiver = dist(receiverIndex)
    val donor = dist(dIndex)
    val beforeDonor = dist(dIndex - 1)
    if donor > receiver + 1 && donor > beforeDonor then Some(dIndex)
    else findDonorIndex(dist, receiverIndex, dIndex + 1)
