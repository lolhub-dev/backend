package lolhub

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import lolhub.GreeterMain.Start

//#main-class
object Main extends App {
  //#actor-system
  val mainActor: ActorSystem[GreeterMain.Start] =
    ActorSystem(GreeterMain(), "LoLHub")
  //#actor-system

  //#main-send-messages
  mainActor ! Start("Charles")
  //#main-send-messages
}
//#main-class
