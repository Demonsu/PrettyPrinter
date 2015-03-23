public class ListPrettyPrinter$
{
  static int apply ()
  {
    class $Doc
    {
      final int tag;
      $Doc (int tag)
      {
        this.tag = tag;
      }
      class $Doc$TXET extends $Doc
      {
        java.lang.String field1;
        $Doc$TXET (java.lang.String field1)
        {
          super(1);
          this.field1 = field1;
        }
      }
      $Doc $Doc$TXET (java.lang.String field1)
      {
        return ($Doc) new $Doc$TXET(field1);
      }
      class $Doc$NEST extends $Doc
      {
        java.lang.Integer field1;
        $Doc field2;
        $Doc$NEST (java.lang.Integer field1, $Doc field2)
        {
          super(2);
          this.field1 = field1;
          this.field2 = field2;
        }
      }
      $Doc $Doc$NEST (java.lang.Integer field1, $Doc field2)
      {
        return ($Doc) new $Doc$NEST(field1, field2);
      }
      $Doc $Doc$LINE;
      $Doc $Doc$LINE ()
      {
        if ($Doc$LINE == null)
          $Doc$LINE = new $Doc(3);
        return $Doc$LINE;
      }
      $Doc $Doc$NIL;
      $Doc $Doc$NIL ()
      {
        if ($Doc$NIL == null)
          $Doc$NIL = new $Doc(4);
        return $Doc$NIL;
      }
      class $Doc$UNION extends $Doc
      {
        $Doc field1;
        $Doc field2;
        $Doc$UNION ($Doc field1, $Doc field2)
        {
          super(5);
          this.field1 = field1;
          this.field2 = field2;
        }
      }
      $Doc $Doc$UNION ($Doc field1, $Doc field2)
      {
        return ($Doc) new $Doc$UNION(field1, field2);
      }
    }
    $Doc $doc = new $Doc(0);
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final java.lang.Integer x3 = (java.lang.Integer) x2.arg;
        class Fun4 extends f2j.Closure
        {
          f2j.Closure x5 = this;
          public void apply ()
          {
            final java.lang.Integer x6 = (java.lang.Integer) x5.arg;
            final java.lang.Integer x7 = x3 + x6;
            res = x7;
          }
        }
        f2j.Closure x4 = new Fun4();
        res = x4;
      }
    }
    f2j.Closure x1 = new Fun1();
    f2j.Closure x8 = x1;
    class $PList
    {
      final int tag;
      $PList (int tag)
      {
        this.tag = tag;
      }
      $PList $PList$Nil;
      $PList $PList$Nil ()
      {
        if ($PList$Nil == null)
          $PList$Nil = new $PList(1);
        return $PList$Nil;
      }
      class $PList$Cons extends $PList
      {
        Object field1;
        $PList field2;
        $PList$Cons (Object field1, $PList field2)
        {
          super(2);
          this.field1 = field1;
          this.field2 = field2;
        }
      }
      $PList $PList$Cons (Object field1, $PList field2)
      {
        return ($PList) new $PList$Cons(field1, field2);
      }
    }
    $PList $plist = new $PList(0);
    class Fun10 extends f2j.Closure
    {
      f2j.Closure x11 = this;
      public void apply ()
      {
        final java.lang.String x12 = (java.lang.String) x11.arg;
        res = x12;
      }
    }
    f2j.Closure x10 = new Fun10();
    f2j.Closure x13 = x10;
    class Fun15 extends f2j.Closure
    {
      f2j.Closure x16 = this;
      public void apply ()
      {
        final f2j.Closure x17 = (f2j.Closure) x16.arg;
        class Fun18 extends f2j.Closure
        {
          f2j.Closure x19 = this;
          public void apply ()
          {
            final Object x20 = x19.arg;
            class Fun21 extends f2j.Closure
            {
              f2j.Closure x22 = this;
              public void apply ()
              {
                final java.lang.String x23 = (java.lang.String) x22.arg;
                f2j.Closure x24 = x17;
                x24.arg = x20;
                x24.apply();
                final java.lang.String x25 = (java.lang.String) x24.res;
                final java.lang.String x26 = x25.<java.lang.String>concat(x23);
                res = x26;
              }
            }
            f2j.Closure x21 = new Fun21();
            res = x21;
          }
        }
        f2j.Closure x18 = new Fun18();
        res = x18;
      }
    }
    f2j.Closure x15 = new Fun15();
    f2j.Closure x27 = x15;
    class Let28
    {
      Object temp;
      f2j.Closure x30;
      {
        class Fun56 extends f2j.Closure
        {
          f2j.Closure x57 = this;
          public void apply ()
          {
            final f2j.Closure x58 = (f2j.Closure) x57.arg;
            class Fun59 extends f2j.Closure
            {
              f2j.Closure x60 = this;
              public void apply ()
              {
                final $PList x61 = ($PList) x60.arg;
                java.lang.String x62;
                switch (x61.tag)
                {
                  case 1:
                    $PList x63 = x61;
                    x62 = "";
                    break;
                  case 2:
                    $PList.$PList$Cons x64 = ($PList.$PList$Cons) x61;
                    Object x65 = x64.field1;
                    $PList x66 = x64.field2;
                    f2j.Closure x73 = x30;
                    x73.arg = x58;
                    x73.apply();
                    final f2j.Closure x74 = (f2j.Closure) x73.res;
                    f2j.Closure x75 = x74;
                    x75.arg = x66;
                    x75.apply();
                    final java.lang.String x76 = (java.lang.String) x75.res;
                    f2j.Closure x68 = x27;
                    x68.arg = x58;
                    x68.apply();
                    final f2j.Closure x69 = (f2j.Closure) x68.res;
                    f2j.Closure x70 = x69;
                    x70.arg = x65;
                    x70.apply();
                    final f2j.Closure x71 = (f2j.Closure) x70.res;
                    f2j.Closure x77 = x71;
                    x77.arg = x76;
                    x77.apply();
                    final java.lang.String x78 = (java.lang.String) x77.res;
                    x62 = x78;
                    break;
                  default:
                    throw new RuntimeException("pattern match fail");
                }
                res = x62;
              }
            }
            f2j.Closure x59 = new Fun59();
            res = x59;
          }
        }
        f2j.Closure x56 = new Fun56();
        x30 = x56;
        $PList x79 = $plist.$PList$Nil();
        $PList x80 = $plist.$PList$Cons("3", x79);
        $PList x81 = $plist.$PList$Cons("2", x80);
        $PList x82 = $plist.$PList$Cons("1", x81);
        $PList x83 = x82;
        class Fun84 extends f2j.Closure
        {
          f2j.Closure x85 = this;
          public void apply ()
          {
            final $Doc x86 = ($Doc) x85.arg;
            class Fun87 extends f2j.Closure
            {
              f2j.Closure x88 = this;
              public void apply ()
              {
                final $Doc x89 = ($Doc) x88.arg;
                $Doc x90 = $doc.$Doc$UNION(x86, x89);
                res = x90;
              }
            }
            f2j.Closure x87 = new Fun87();
            res = x87;
          }
        }
        f2j.Closure x84 = new Fun84();
        f2j.Closure x91 = x84;
        class Fun92 extends f2j.Closure
        {
          f2j.Closure x93 = this;
          public void apply ()
          {
            final java.lang.Integer x94 = (java.lang.Integer) x93.arg;
            class Fun95 extends f2j.Closure
            {
              f2j.Closure x96 = this;
              public void apply ()
              {
                final java.lang.Integer x97 = (java.lang.Integer) x96.arg;
                final java.lang.Integer x98 = x94 + x97;
                res = x98;
              }
            }
            f2j.Closure x95 = new Fun95();
            res = x95;
          }
        }
        f2j.Closure x92 = new Fun92();
        f2j.Closure x99 = x92;
        f2j.Closure x100 = x8;
        x100.arg = 1;
        x100.apply();
        final f2j.Closure x101 = (f2j.Closure) x100.res;
        f2j.Closure x102 = x101;
        x102.arg = 2;
        x102.apply();
        final java.lang.Integer x103 = (java.lang.Integer) x102.res;
        temp = x103;
      }
    }
    Let28 x28 = new Let28();
    final java.lang.Integer x29 = (java.lang.Integer) x28.temp;
    return x29;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}