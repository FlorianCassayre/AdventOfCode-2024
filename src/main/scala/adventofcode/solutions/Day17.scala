package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day17 = Day(17) { (input, part) =>

  val A = 0
  val B = 1
  val C = 2

  def comboOperand(state: State, operand: Int): Long =
    val registersOffset = 4
    if operand < registersOffset then operand else state.registers(operand - registersOffset)

  case class State(ip: Int, registers: IndexedSeq[Long], output: IndexedSeq[Long]):
    def apply(i: Int): Long = registers(i)
    def updated(i: Int, value: Long): State = copy(registers = registers.updated(i, value))
    def next: State = copy(ip = ip + 2)
    def execute(instruction: Int, operand: Int): State = instruction match
      case 0 /* adv */ => updated(A, apply(A) / (1L << comboOperand(this, operand))).next
      case 1 /* bxl */ => updated(B, apply(B) ^ operand).next
      case 2 /* bst */ => updated(B, comboOperand(this, operand) % 8).next
      case 3 /* jnz */ => if apply(A) == 0 then next else copy(ip = operand)
      case 4 /* bxc */ => updated(B, apply(B) ^ apply(C)).next
      case 5 /* out */ => copy(output = output :+ (comboOperand(this, operand) % 8)).next
      case 6 /* bdv */ => updated(B, apply(A) / (1L << comboOperand(this, operand))).next
      case 7 /* cdv */ => updated(C, apply(A) / (1L << comboOperand(this, operand))).next

  val (initialRegisters, program) = input.toLines match
    case Seq(s"Register A: $a", s"Register B: $b", s"Register C: $c", "", s"Program: $p") =>
      (IndexedSeq(a.toLong, b.toLong, c.toLong), p.split(",").map(_.toInt).toIndexedSeq)

  val initialState = State(0, initialRegisters, IndexedSeq.empty)

  def execute(state: State, program: IndexedSeq[Int]): State =
    if program.indices.contains(state.ip) && program.indices.contains(state.ip + 1) then
      execute(state.execute(program(state.ip), program(state.ip + 1)), program)
    else
      state

  part(1) = execute(initialState, program).output.mkString(",")

  part(2) = 105981155568026L.ensuring(quine => execute(initialState.updated(A, quine), program).output == program)

}
