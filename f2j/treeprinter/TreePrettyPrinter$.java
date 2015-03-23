public class TreePrettyPrinter$
{
  static Object apply ()
  {
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
    class $BTree
    {
      final int tag;
      $BTree (int tag)
      {
        this.tag = tag;
      }
      class $BTree$Node extends $BTree
      {
        java.lang.String field1;
        $PList field2;
        $BTree$Node (java.lang.String field1, $PList field2)
        {
          super(1);
          this.field1 = field1;
          this.field2 = field2;
        }
      }
      $BTree $BTree$Node (java.lang.String field1, $PList field2)
      {
        return ($BTree) new $BTree$Node(field1, field2);
      }
    }
    $BTree $btree = new $BTree(0);
    class Fun2 extends f2j.Closure
    {
      f2j.Closure x3 = this;
      public void apply ()
      {
        final java.lang.String x4 = (java.lang.String) x3.arg;
        class Fun5 extends f2j.Closure
        {
          f2j.Closure x6 = this;
          public void apply ()
          {
            final java.lang.String x7 = (java.lang.String) x6.arg;
            final java.lang.String x8 = x4.<java.lang.String>concat(x7);
            res = x8;
          }
        }
        f2j.Closure x5 = new Fun5();
        res = x5;
      }
    }
    f2j.Closure x2 = new Fun2();
    f2j.Closure x9 = x2;
    class Let10
    {
      Object temp;
      f2j.Closure x12;
      f2j.Closure x13;
      {
        class Fun47 extends f2j.Closure
        {
          f2j.Closure x48 = this;
          public void apply ()
          {
            final $BTree x49 = ($BTree) x48.arg;
            java.lang.String x50;
            switch (x49.tag)
            {
              case 1:
                $BTree.$BTree$Node x51 = ($BTree.$BTree$Node) x49;
                java.lang.String x52 = x51.field1;
                $PList x53 = x51.field2;
                f2j.Closure x56 = x13;
                x56.arg = x53;
                x56.apply();
                final java.lang.String x57 = (java.lang.String) x56.res;
                f2j.Closure x54 = x9;
                x54.arg = x52;
                x54.apply();
                final f2j.Closure x55 = (f2j.Closure) x54.res;
                f2j.Closure x58 = x55;
                x58.arg = x57;
                x58.apply();
                final java.lang.String x59 = (java.lang.String) x58.res;
                x50 = x59;
                break;
              default:
                throw new RuntimeException("pattern match fail");
            }
            res = x50;
          }
        }
        f2j.Closure x47 = new Fun47();
        x12 = x47;
        class Fun60 extends f2j.Closure
        {
          f2j.Closure x61 = this;
          public void apply ()
          {
            final $PList x62 = ($PList) x61.arg;
            java.lang.String x63;
            switch (x62.tag)
            {
              case 1:
                $PList x64 = x62;
                x63 = "";
                break;
              case 2:
                $PList.$PList$Cons x65 = ($PList.$PList$Cons) x62;
                $BTree x66 = ($BTree) x65.field1;
                $PList x67 = x65.field2;
                f2j.Closure x76 = x13;
                x76.arg = x67;
                x76.apply();
                final java.lang.String x77 = (java.lang.String) x76.res;
                f2j.Closure x68 = x12;
                x68.arg = x66;
                x68.apply();
                final java.lang.String x69 = (java.lang.String) x68.res;
                f2j.Closure x70 = x9;
                x70.arg = x69;
                x70.apply();
                final f2j.Closure x71 = (f2j.Closure) x70.res;
                f2j.Closure x72 = x71;
                x72.arg = ",";
                x72.apply();
                final java.lang.String x73 = (java.lang.String) x72.res;
                f2j.Closure x74 = x9;
                x74.arg = x73;
                x74.apply();
                final f2j.Closure x75 = (f2j.Closure) x74.res;
                f2j.Closure x78 = x75;
                x78.arg = x77;
                x78.apply();
                final java.lang.String x79 = (java.lang.String) x78.res;
                x63 = x79;
                break;
              default:
                throw new RuntimeException("pattern match fail");
            }
            res = x63;
          }
        }
        f2j.Closure x60 = new Fun60();
        x13 = x60;
        $PList x80 = $plist.$PList$Nil();
        $BTree x81 = $btree.$BTree$Node("bbbbb", x80);
        $PList x82 = $plist.$PList$Nil();
        $BTree x83 = $btree.$BTree$Node("eee", x82);
        $PList x84 = $plist.$PList$Nil();
        $BTree x85 = $btree.$BTree$Node("ffff", x84);
        $PList x86 = $plist.$PList$Nil();
        $PList x87 = $plist.$PList$Cons(x85, x86);
        $PList x88 = $plist.$PList$Cons(x83, x87);
        $PList x89 = $plist.$PList$Cons(x81, x88);
        $BTree x90 = $btree.$BTree$Node("aaa", x89);
        $BTree x91 = x90;
        f2j.Closure x92 = x12;
        x92.arg = x91;
        x92.apply();
        final java.lang.String x93 = (java.lang.String) x92.res;
        temp = x93;
      }
    }
    Let10 x10 = new Let10();
    final java.lang.String x11 = (java.lang.String) x10.temp;
    return x11;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}