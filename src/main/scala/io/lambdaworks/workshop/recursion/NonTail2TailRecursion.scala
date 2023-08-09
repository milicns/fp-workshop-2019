package io.lambdaworks.workshop.recursion

import scala.annotation.tailrec

/**
  * Rewrite below non tail-recursive functions to tail-recursive one.
  * Add @tailrec annotation to prove it.
  */
object NonTail2TailRecursion {

  def factorial(n: Int): Int = {
    @tailrec
    def loop(product: Int, n: Int): Int =
      if (n == 1) product else loop(product * n, n - 1)

    loop(1, n)
  }

  def cubesOfEvens(numbers: List[Double]): List[Double] = {
    @tailrec
    def loop(numbers: List[Double], cubes: List[Double]): List[Double] =
      numbers match {
        case Nil => cubes
        case x :: xs if x % 2 == 0 => loop(xs, Math.pow(x, 3) +: cubes)
        case _ :: xs => loop(xs, cubes)
      }

    loop(numbers, List.empty)
  }

}
