import complete.Parsers._

name := "Advent Of Code 2017"

lazy val day = inputKey[Unit]("Runs Day with given number")

def runDay(dayNumber: Int) = runMain fullInput s"aoc2017.Day${"%02d" format dayNumber}"

day := Def.inputTaskDyn {
  val dayNum = IntBasic.parsed
  println(s"Running Day $dayNum")
  Def.taskDyn {
    (runMain in Compile).toTask(s" aoc2017.Day${"%02d" format dayNum}")
  }
}.evaluated