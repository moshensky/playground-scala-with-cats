package printable

object PrintableSyntax {
  implicit class PrintableOpts[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(format(p))
  }
}
