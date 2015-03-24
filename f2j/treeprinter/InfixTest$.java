public class InfixTest$
{
  static Object apply ()
  {
    class $Doc
    {
      final int tag;
      $Doc (int tag)
      {
        this.tag = tag;
      }
      $Doc $Doc$Nil;
      $Doc $Doc$Nil ()
      {
        if ($Doc$Nil == null)
          $Doc$Nil = new $Doc(1);
        return $Doc$Nil;
      }
      class $Doc$TEXT extends $Doc
      {
        java.lang.String field1;
        $Doc field2;
        $Doc$TEXT (java.lang.String field1, $Doc field2)
        {
          super(2);
          this.field1 = field1;
          this.field2 = field2;
        }
      }
      $Doc $Doc$TEXT (java.lang.String field1, $Doc field2)
      {
        return ($Doc) new $Doc$TEXT(field1, field2);
      }
      class $Doc$LINE extends $Doc
      {
        java.lang.Integer field1;
        $Doc field2;
        $Doc$LINE (java.lang.Integer field1, $Doc field2)
        {
          super(3);
          this.field1 = field1;
          this.field2 = field2;
        }
      }
      $Doc $Doc$LINE (java.lang.Integer field1, $Doc field2)
      {
        return ($Doc) new $Doc$LINE(field1, field2);
      }
    }
    $Doc $doc = new $Doc(0);
    class Let1
    {
      Object temp;
      f2j.Closure x3;
      {
        class Fun28 extends f2j.Closure
        {
          f2j.Closure x29 = this;
          public void apply ()
          {
            final $Doc x30 = ($Doc) x29.arg;
            class Fun31 extends f2j.Closure
            {
              f2j.Closure x32 = this;
              public void apply ()
              {
                final $Doc x33 = ($Doc) x32.arg;
                $Doc x34;
                switch (x30.tag)
                {
                  case 2:
                    $Doc.$Doc$TEXT x35 = ($Doc.$Doc$TEXT) x30;
                    java.lang.String x36 = x35.field1;
                    $Doc x37 = x35.field2;
                    f2j.Closure x38 = x3;
                    x38.arg = x37;
                    x38.apply();
                    final f2j.Closure x39 = (f2j.Closure) x38.res;
                    f2j.Closure x40 = x39;
                    x40.arg = x33;
                    x40.apply();
                    final $Doc x41 = ($Doc) x40.res;
                    $Doc x42 = $doc.$Doc$TEXT(x36, x41);
                    x34 = x42;
                    break;
                  case 3:
                    $Doc.$Doc$LINE x43 = ($Doc.$Doc$LINE) x30;
                    java.lang.Integer x44 = x43.field1;
                    $Doc x45 = x43.field2;
                    f2j.Closure x46 = x3;
                    x46.arg = x45;
                    x46.apply();
                    final f2j.Closure x47 = (f2j.Closure) x46.res;
                    f2j.Closure x48 = x47;
                    x48.arg = x33;
                    x48.apply();
                    final $Doc x49 = ($Doc) x48.res;
                    $Doc x50 = $doc.$Doc$LINE(x44, x49);
                    x34 = x50;
                    break;
                  case 1:
                    $Doc x51 = x30;
                    x34 = x33;
                    break;
                  default:
                    throw new RuntimeException("pattern match fail");
                }
                res = x34;
              }
            }
            f2j.Closure x31 = new Fun31();
            res = x31;
          }
        }
        f2j.Closure x28 = new Fun28();
        x3 = x28;
        class Fun52 extends f2j.Closure
        {
          f2j.Closure x53 = this;
          public void apply ()
          {
            final java.lang.String x54 = (java.lang.String) x53.arg;
            class Fun55 extends f2j.Closure
            {
              f2j.Closure x56 = this;
              public void apply ()
              {
                final java.lang.String x57 = (java.lang.String) x56.arg;
                final java.lang.String x58 = x54.<java.lang.String>concat(x57);
                res = x58;
              }
            }
            f2j.Closure x55 = new Fun55();
            res = x55;
          }
        }
        f2j.Closure x52 = new Fun52();
        f2j.Closure x59 = x52;
        $Doc x60 = $doc.$Doc$Nil();
        $Doc x61 = $doc.$Doc$LINE(2, x60);
        $Doc x62 = $doc.$Doc$TEXT("a", x61);
        $Doc x63 = x62;
        $Doc x64 = $doc.$Doc$Nil();
        $Doc x65 = $doc.$Doc$LINE(3, x64);
        $Doc x66 = $doc.$Doc$TEXT("b", x65);
        $Doc x67 = x66;
        class Let68
        {
          Object temp;
          f2j.Closure x70;
          {
            class Fun94 extends f2j.Closure
            {
              f2j.Closure x95 = this;
              public void apply ()
              {
                final $Doc x96 = ($Doc) x95.arg;
                java.lang.String x97;
                switch (x96.tag)
                {
                  case 2:
                    $Doc.$Doc$TEXT x98 = ($Doc.$Doc$TEXT) x96;
                    java.lang.String x99 = x98.field1;
                    $Doc x100 = x98.field2;
                    f2j.Closure x103 = x70;
                    x103.arg = x100;
                    x103.apply();
                    final java.lang.String x104 = (java.lang.String) x103.res;
                    f2j.Closure x101 = x59;
                    x101.arg = x99;
                    x101.apply();
                    final f2j.Closure x102 = (f2j.Closure) x101.res;
                    f2j.Closure x105 = x102;
                    x105.arg = x104;
                    x105.apply();
                    final java.lang.String x106 = (java.lang.String) x105.res;
                    x97 = x106;
                    break;
                  case 3:
                    $Doc.$Doc$LINE x107 = ($Doc.$Doc$LINE) x96;
                    java.lang.Integer x108 = x107.field1;
                    $Doc x109 = x107.field2;
                    f2j.Closure x112 = x70;
                    x112.arg = x109;
                    x112.apply();
                    final java.lang.String x113 = (java.lang.String) x112.res;
                    f2j.Closure x110 = x59;
                    x110.arg = "\n";
                    x110.apply();
                    final f2j.Closure x111 = (f2j.Closure) x110.res;
                    f2j.Closure x114 = x111;
                    x114.arg = x113;
                    x114.apply();
                    final java.lang.String x115 = (java.lang.String) x114.res;
                    x97 = x115;
                    break;
                  case 1:
                    $Doc x116 = x96;
                    x97 = "";
                    break;
                  default:
                    throw new RuntimeException("pattern match fail");
                }
                res = x97;
              }
            }
            f2j.Closure x94 = new Fun94();
            x70 = x94;
            f2j.Closure x117 = x3;
            x117.arg = x63;
            x117.apply();
            final f2j.Closure x118 = (f2j.Closure) x117.res;
            f2j.Closure x119 = x118;
            x119.arg = x67;
            x119.apply();
            final $Doc x120 = ($Doc) x119.res;
            f2j.Closure x121 = x70;
            x121.arg = x120;
            x121.apply();
            final java.lang.String x122 = (java.lang.String) x121.res;
            temp = x122;
          }
        }
        Let68 x68 = new Let68();
        final java.lang.String x69 = (java.lang.String) x68.temp;
        temp = x69;
      }
    }
    Let1 x1 = new Let1();
    final java.lang.String x2 = (java.lang.String) x1.temp;
    return x2;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}