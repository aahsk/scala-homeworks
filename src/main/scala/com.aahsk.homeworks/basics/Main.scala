package com.aahsk.homeworks.basics

object Basics {
  // Algorithm sourced from https://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor
  def lcm(a: Int, b: Int): Int = Math.abs(a * b) / gcd(a, b)

  // Algorithm sourced from https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclid.27s_algorithm
  def gcd(a: Int, b: Int): Int = b match {
    case 0 => Math.abs(a)
    case _ => gcd(b, a % b)
  }
}