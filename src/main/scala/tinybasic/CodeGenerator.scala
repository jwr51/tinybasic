package tinybasic

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

import scala.collection.mutable

object CodeGenerator {

  def apply(title: String, program: Program): Array[Byte] = {
    val gen = new CodeGenerator(title, program)
    gen.generate()
  }
}

private class CodeGenerator(val title: String, program: Program) {

  private val clazz = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
  private val method = clazz.visitMethod(ACC_PRIVATE | ACC_STATIC, "program", "()V", null, null)

  private val lines = program.lines

  // + return label
  private val labels = List.fill(lines.size + 1)(new Label())
  private val lineLabels = lines.map(_.num).zip(labels).toMap
  private val locals = mutable.Map[String, Int]()

  def labelByLine(line: Int): Label = lineLabels(line)

  def local(name: String): Int = locals.getOrElseUpdate(name, locals.size)

  def next(label: Label): Label = labels(labels.indexOf(label) + 1)

  def generate(): Array[Byte] = {
    clazz.visit(V10, ACC_PUBLIC, title, null, "java/lang/Object", null)

    val main = clazz.visitMethod(ACC_PUBLIC | ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    main.visitCode()
    main.visitMethodInsn(INVOKESTATIC, title, "program", "()V", false)
    main.visitInsn(RETURN)

    program.lines.foreach(line => {
      val label = labelByLine(line.num)
      method.visitLabel(label)
      visit(label, line.statement)
    })

    val exit = labels.last
    method.visitLabel(exit)
    method.visitInsn(RETURN)

    method.visitMaxs(-1, -1)
    method.visitEnd()

    clazz.visitEnd()
    clazz.toByteArray
  }

  def visit(label: Label, statement: Statement): Unit = {
    statement match {
      case Assign(name, value)      =>
        visit(value)
        method.visitVarInsn(ISTORE, local(name))
      case End                      =>
        method.visitInsn(ICONST_0)
        method.visitMethodInsn(INVOKESTATIC, "java/lang/System", "exit", "(I)V", false)
      case If(op, lhs, rhs, body)   =>
        visit(lhs)
        visit(rhs)

        // jump to next line if condition not met
        val opcode = op match {
          case Eq => IF_ICMPNE
          case Ne => IF_ICMPEQ
          case Gt => IF_ICMPLE
          case Ge => IF_ICMPLT
          case Lt => IF_ICMPGE
          case Le => IF_ICMPGT
        }
        method.visitJumpInsn(opcode, next(label))

        // otherwise continue to body of if-statement
        visit(label, body)
      case Input(vars)              =>
        vars.foreach(v => {
          method.visitTypeInsn(NEW, "java/util/Scanner")
          method.visitInsn(DUP)
          method.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;")
          method.visitMethodInsn(INVOKESPECIAL, "java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V", false)
          method.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextInt", "()I", false)
          method.visitVarInsn(ISTORE, local(v))
        })
      case Goto(IntConst(address))  => method.visitJumpInsn(GOTO, labelByLine(address))
      case Gosub(IntConst(address)) => throw new UnsupportedOperationException("Subroutines not yet implemented.")
      case Goto(_) | Gosub(_)       => throw new UnsupportedOperationException("Non-constant jumps.")
      case Print(exprs)             =>
        exprs.foreach(expr => {
          method.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
          visit(expr)

          val desc = expr match {
            case StringConst(_) => "(Ljava/lang/String;)V"
            case _              => "(I)V"
          }
          method.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", desc, false)
        })
      case Return                   => throw new UnsupportedOperationException(s"Subroutines not yet implemented.")
    }
  }

  def visit(expr: Expr): Unit = {
    expr match {
      case BinaryOp(op, left, right) =>
        visit(left)
        visit(right)

        val opcode = op match {
          case AddOp => IADD
          case SubOp => ISUB
          case MulOp => IMUL
          case DivOp => IDIV
        }
        method.visitInsn(opcode)
      case IntConst(value)           => method.visitLdcInsn(value)
      case StringConst(value)        => method.visitLdcInsn(value)
      case Negate(op)                =>
        visit(op)
        method.visitInsn(INEG)
      case Var(name)                 => method.visitVarInsn(ILOAD, local(name))
    }
  }
}