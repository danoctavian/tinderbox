package models.bot.tasks.message

/**
 * A list of fun, pre-defined message trees.
 */
object FunMessages {

  def messages = List(
    MessageTree(
      value = "{name} are you a fan of avocados?",
      right = Some(MessageTree(
        value = "So if I asked you to have a guacamole party with me you'd do it?",
        right = None,
        left = None
      )),
      left = Some(MessageTree(
        value = "Do women love anything more than avocados?",
        right = None,
        left = None
      ))
    ),

    MessageTree(
      value = "hey {name}, i have a mission and looking for a sidekick, you down?",
      right = None,
      left = None)
  )
}
