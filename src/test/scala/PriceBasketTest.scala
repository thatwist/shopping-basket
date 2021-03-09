import wvlet.airspec._

class PriceBasketTest extends AirSpec {

  def `empty basket should produce 0 total`: Unit = {
    val basket = new Basket(Nil, Nil)
    assert(basket.finalPrice == 0)
  }

  def `bread should be 50% off for each 2 tins of soup`: Unit = {
    val bread = PriceBasket.Products.find(_.name == "Bread").get
    val soup = PriceBasket.Products.find(_.name == "Soup").get
    val basket1 = new Basket(List(bread, bread, soup, soup), PriceBasket.Offers)
    assert(basket1.finalPrice == 2.5)
    val basket2 = new Basket(List(bread, bread, soup, soup, soup, soup), PriceBasket.Offers)
    assert(basket2.finalPrice == 3.4)
  }

  def `basket should be described in the required format`: Unit = {
    val apples = PriceBasket.Products.find(_.name == "Apples").get
    val basket = new Basket(List(apples), PriceBasket.Offers)
    assert(basket.describe ==
      """Subtotal: £1.0
        |Apples 10% off: 10p
        |Total price: 90p""".stripMargin)
  }

}
