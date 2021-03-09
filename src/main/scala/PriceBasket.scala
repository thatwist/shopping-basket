import scala.math.BigDecimal

case class Product(name: String, price: BigDecimal, description: String)

case class SpecialOffer(discount: List[Product] => BigDecimal, shortDescription: String, fullDescription: String)

class Basket(val items: List[Product], offers: List[SpecialOffer]) {
  def subtotal: BigDecimal = items.map(_.price).toList.sum
  def finalPrice: BigDecimal = subtotal - offers.foldLeft(BigDecimal(0))((sum, offer) => sum + offer.discount(items))
  def describe: String = {
    val offersFormatted = offers
      .map(off => off.shortDescription -> off.discount(items))
      .filter(_._2 > 0) // filter out not applicable offers
      .map { case (description, amount) => s"$description: ${Pounds.format(amount)}" }
    val offersDescription = if (offersFormatted.nonEmpty)
      offersFormatted.mkString("\n")
    else "(No offers available) "
    s"""Subtotal: ${Pounds.format(subtotal)}
    |$offersDescription
    |Total price: ${Pounds.format(finalPrice)}""".stripMargin
  }
}
 
object Pounds {
  def format(amount: BigDecimal): String = {
    amount.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)
    if (amount < 1) s"${(amount * 100).toInt}p" else s"£$amount"
  }
}

object PriceBasket {

  val Products: List[Product] = List(
    Product("Soup", 0.65, "per tin"),
    Product("Bread", 0.80, "per loaf"),
    Product("Milk", 1.30, "per bottle"),
    Product("Apples", 1.00, "per bag"))

  val Offers: List[SpecialOffer] = List(SpecialOffer(
    _.filter(_.name == "Apples").map(_.price).sum * 0.1,
    "Apples 10% off", "Apples have a 10% discount off their normal price this week"
  ), SpecialOffer(
    products => products.filter(_.name == "Bread").take(products.count(_.name == "Soup") / 2).map(_.price).sum * 0.5,
    "Bread 50% off for 2 tins of soup",
    "Buy 2 tins of soup and get a loaf of bread for half price"
  ))

  def main(args: Array[String]): Unit = {
    val basket = new Basket(args.toList.map(name => Products.find(_.name == name)).flatten, Offers)
    println(basket.describe)
  }

  /*
   possible improvements
    - better support currencies
    - dsl for creating special offers
   */
}
