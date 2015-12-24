object TypeParamSample {
  class TypeParam[T](val t:T){
    def get: T = this.t
  }

  def main(args: Array[String]): Unit = {
    val stringTypePram = new TypeParam[String]("test");
    println(stringTypePram.get);

    val intTypePram = new TypeParam[Int](1);
    println(intTypePram.get);
  }
}
