package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {
  import Day18._

  test("explode") {
    val a = parse("[[[[[9,8],1],2],3],4]")
    explode(a)
    assert(a.toString === "[[[[0,9],2],3],4]")
    val b = parse("[7,[6,[5,[4,[3,2]]]]]")
    explode(b)
    assert(b.toString === "[7,[6,[5,[7,0]]]]")
    val c = parse("[[6,[5,[4,[3,2]]]],1]")
    explode(c)
    assert(c.toString === "[[6,[5,[7,0]]],3]")
    val d = parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
    explode(d)
    assert(d.toString === "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    val e = parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    explode(e)
    assert(e.toString === "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  }

  test("example 0") {
    val a = parse("[[[[4,3],4],4],[7,[[8,4],9]]]")
    val b = parse("[1,1]")
    val c = snailfishSum(a, b)
    assert(c.toString === "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }

  test("example 1") {
    assert(fullSum(
      """[1,1]
        |[2,2]
        |[3,3]
        |[4,4]""".stripMargin.split("\n")).toString === "[[[[1,1],[2,2]],[3,3]],[4,4]]")
  }

  test("example 2") {
    assert(fullSum(
      """[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |[5,5]""".stripMargin.split("\n")).toString === "[[[[3,0],[5,3]],[4,4]],[5,5]]")
  }

  test("example 3") {
    assert(fullSum(
      """[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |[5,5]
        |[6,6]""".stripMargin.split("\n")).toString === "[[[[5,0],[7,4]],[5,5]],[6,6]]")
  }

  val bigExample =
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
      |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
      |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
      |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
      |[7,[5,[[3,8],[1,4]]]]
      |[[2,[2,2]],[8,[8,1]]]
      |[2,9]
      |[1,[[[9,3],9],[[9,0],[0,7]]]]
      |[[[5,[7,4]],7],1]
      |[[[[4,2],2],6],[8,7]]""".stripMargin.split("\n").map(parse)

  test("larger example 1") {
    val sum1 = "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    assert(snailfishSum(bigExample(0), bigExample(1)).toString === sum1)
  }

  test("larger example 2") {
    val sum1 = "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    val sum2 = "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
    assert(snailfishSum(parse(sum1), bigExample(2)).toString === sum2)
  }

  test("larger example 3") {
    val sum2 = "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
    val sum3 = "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
    assert(snailfishSum(parse(sum2), bigExample(3)).toString === sum3)
  }

  test("larger example 4") {
    val sum3 = "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
    val sum4 = "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
    assert(snailfishSum(parse(sum3), bigExample(4)).toString === sum4)
  }

  test("larger example 5") {
    val sum4 = "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
    val sum5 = "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
    assert(snailfishSum(parse(sum4), bigExample(5)).toString === sum5)
  }

  test("larger example 6") {
    val sum5 = "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
    val sum6 = "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
    assert(snailfishSum(parse(sum5), bigExample(6)).toString === sum6)
  }

  test("larger example 7") {
    val sum6 = "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
    val sum7 = "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
    assert(snailfishSum(parse(sum6), bigExample(7)).toString === sum7)
  }

  test("larger example 8") {
    val sum7 = "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
    val sum8 = "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
    assert(snailfishSum(parse(sum7), bigExample(8)).toString === sum8)
  }

  test("larger example 9") {
    val sum8 = "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
    val sum9 = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    assert(snailfishSum(parse(sum8), bigExample(9)).toString === sum9)
  }

  test("magnitude") {
    val a = parse("[9,1]")
    assert(magnitude(a) === 29)
    val b = parse("[[9,1],[1,9]]")
    assert(magnitude(b) === 129)
    val f = parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
    assert(magnitude(f) === 3488)
  }

  test("homework") {
    val input =
      """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
        |[[[5,[2,8]],4],[5,[[9,9],0]]]
        |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
        |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
        |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
        |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
        |[[[[5,4],[7,7]],8],[[8,3],8]]
        |[[9,3],[[9,9],[6,[4,9]]]]
        |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
        |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.split("\n")

    val x = fullSum(input)
    assert(x.toString === "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
    assert(magnitude(x) === 4140)
  }
}
