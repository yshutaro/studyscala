class Cafe {
  def buyCoffee(cc: CreditCard): Coffee = {
    val cpu = new Coffee()
    cc.charge(cup.price)
    cup
  }
}
