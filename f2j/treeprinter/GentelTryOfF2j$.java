public class GentelTryOfF2j$
{
  static Object apply ()
  {
    class Let1
    {
      Object temp;
      f2j.Closure x3;
      {
        class Fun16 extends f2j.Closure
        {
          f2j.Closure x17 = this;
          public void apply ()
          {
            final java.math.BigInteger x18 = (java.math.BigInteger) x17.arg;
            final java.lang.Integer x20 = x18.intValue();
            final java.lang.Boolean x21 = x20 == 0;
            java.math.BigInteger ifres19;
            if (x21)
            {
              final java.math.BigInteger x22 = new <java.lang.String> java.math.BigInteger("1");
              ifres19 = x22;
            }
            else
            {
              final java.math.BigInteger x23 = new <java.lang.String> java.math.BigInteger("1");
              final java.math.BigInteger x24 = x18.<java.math.BigInteger>subtract(x23);
              f2j.Closure x25 = x3;
              x25.arg = x24;
              x25.apply();
              final java.math.BigInteger x26 = (java.math.BigInteger) x25.res;
              final java.math.BigInteger x27 = x18.<java.math.BigInteger>multiply(x26);
              ifres19 = x27;
            }
            res = ifres19;
          }
        }
        f2j.Closure x16 = new Fun16();
        x3 = x16;
        final java.math.BigInteger x28 = new <java.lang.String> java.math.BigInteger("20");
        f2j.Closure x29 = x3;
        x29.arg = x28;
        x29.apply();
        final java.math.BigInteger x30 = (java.math.BigInteger) x29.res;
        temp = x30;
      }
    }
    Let1 x1 = new Let1();
    final java.math.BigInteger x2 = (java.math.BigInteger) x1.temp;
    return x2;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}