import scalaj.http.Http

object Main {
  val api_key:String = "0854fb0e3ca404cbd1d52d407837b669038ed4b6a83cd8db83f71a1ea2780fa8"
  val url_format:String = "https://min-api.cryptocompare.com/data/pricemulti?fsyms=%s&tsyms=%s&api_key=%s"
  val coin_list = List("BTC", "ETH", "DOGE", "BNB", "ADA", "XRP", "AXS", "BUSD", "DOT", "USDT", "ETC", "LTC", "LINK", "EOS", "DASH", "FIL", "THETA")
  
  def main(args: Array[String]): Unit = {
      var coins = List("BTC", "ETH", "DOGE", "BNB", "ADA", "XRP", "AXS", "BUSD", "DOT", "USDT", "ETC", "LTC")
      val logo = scala.io.Source.fromFile("src/resources/app_logo.txt").mkString
      println(" ")
      println(logo)
      runAppLoop(coins)
  }
  
  def runAppLoop(c:List[String]): Unit = {
        println("Commands: v -> View, e -> edit coins, q -> quit")
        var input = scala.io.StdIn.readLine("Your action: ")
        var cc = c
        input match {
          case "v" => {
              //Yo
              if(c.size != 0)
                view(cc)
              else
                println("No coin is active...")
          }
          case "e" => {
              cc = editCoin(c)
          }
          case "q" => return
        }
        runAppLoop(cc)
  }

  def editCoin(c:List[String]): List[String] = {
      showCoinList(coin_list, c)
      println("Commands: b -> Go back, [number] -> enable/disable")
      var input = scala.io.StdIn.readLine("Your actions: ")
      input match {
          case "b" => {
              return c
          }
          case _ => {
              var s:Int = -1
              try {
                s = input.toInt
              } catch {
                case e: Exception => {}
              }

              if(s > 0 && s <= coin_list.size) {
                if(!(c.contains(coin_list(s-1)))) {
                  val cc = c :+ coin_list(s-1)
                  return editCoin(cc)
                }
                else {
                  val cc = c.filter(_ != coin_list(s-1))
                  return editCoin(cc)
                }                
              }
              
          }
      }
      editCoin(c)
  }

  def showCoinList(c_list:List[String], c:List[String], index:Integer = 0): Unit = {
      c_list match {
       case Nil  => return
       case h :: t => {
         val i = index + 1
         if(c.contains(h))
           println(s"[$i] $h -> Active")
         else
           println(s"[$i] $h -> Not Active")
         showCoinList(t, c, i)
       }
     }
  }
  
  def viewCoin(c:List[String]): Unit = {
      val response = Http(url_format.format(c.mkString(","), "USD,THB,JPY,EUR", api_key)).asString
      val data = ujson.read(response.body)
      println(" ")
      println("-----------------------------------------------------------------------------------")
      println(String.format( "%10s %17s %17s %17s %17s",  "Currency", "Price(USD)", "Price(THB)","Price(JPY)","Price(EUR)" ))
      println("-----------------------------------------------------------------------------------")
      c.map(x => {
          println(String.format( "%10s %17s %17s %17s %17s",  x, data(x)("USD"), data(x)("THB"), data(x)("JPY"), data(x)("EUR") ))
        }
      )
      println("-----------------------------------------------------------------------------------")
  }

//free account can only query up to 250,000 times.
  def view(c:List[String]): Unit = {
      var input:String = ""
      val thread = new Thread {
        override def run {
            try {
              while(true) {
                viewCoin(c)
                println("Auto updating every 5 seconds. Press enter to stop...")
                Thread.sleep(5000)
              }
            }
            catch {
              case _: Throwable => {}
            }
        }
      }
      thread.start
      scala.io.StdIn.readLine("")
      thread.stop
  }
  

}