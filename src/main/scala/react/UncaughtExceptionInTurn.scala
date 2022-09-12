package react

class UncaughtExceptionInTurn(message: String, turn: Int, cause: Throwable) extends RuntimeException(s"$message, turn = $turn", cause)
