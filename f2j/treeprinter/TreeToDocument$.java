public class TreeToDocument$
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
    $Doc x1 = $doc.$Doc$Nil();
    $Doc x2 = x1;
    class Let3
    {
      Object temp;
      f2j.Closure x5;
      {
        class Fun30 extends f2j.Closure
        {
          f2j.Closure x31 = this;
          public void apply ()
          {
            final $Doc x32 = ($Doc) x31.arg;
            class Fun33 extends f2j.Closure
            {
              f2j.Closure x34 = this;
              public void apply ()
              {
                final $Doc x35 = ($Doc) x34.arg;
                $Doc x36;
                switch (x32.tag)
                {
                  case 2:
                    $Doc.$Doc$TEXT x37 = ($Doc.$Doc$TEXT) x32;
                    java.lang.String x38 = x37.field1;
                    $Doc x39 = x37.field2;
                    f2j.Closure x40 = x5;
                    x40.arg = x39;
                    x40.apply();
                    final f2j.Closure x41 = (f2j.Closure) x40.res;
                    f2j.Closure x42 = x41;
                    x42.arg = x35;
                    x42.apply();
                    final $Doc x43 = ($Doc) x42.res;
                    $Doc x44 = $doc.$Doc$TEXT(x38, x43);
                    x36 = x44;
                    break;
                  case 3:
                    $Doc.$Doc$LINE x45 = ($Doc.$Doc$LINE) x32;
                    java.lang.Integer x46 = x45.field1;
                    $Doc x47 = x45.field2;
                    f2j.Closure x48 = x5;
                    x48.arg = x47;
                    x48.apply();
                    final f2j.Closure x49 = (f2j.Closure) x48.res;
                    f2j.Closure x50 = x49;
                    x50.arg = x35;
                    x50.apply();
                    final $Doc x51 = ($Doc) x50.res;
                    $Doc x52 = $doc.$Doc$LINE(x46, x51);
                    x36 = x52;
                    break;
                  case 1:
                    $Doc x53 = x32;
                    x36 = x35;
                    break;
                  default:
                    throw new RuntimeException("pattern match fail");
                }
                res = x36;
              }
            }
            f2j.Closure x33 = new Fun33();
            res = x33;
          }
        }
        f2j.Closure x30 = new Fun30();
        x5 = x30;
        class Let54
        {
          Object temp;
          f2j.Closure x56;
          {
            class Fun83 extends f2j.Closure
            {
              f2j.Closure x84 = this;
              public void apply ()
              {
                final java.lang.Integer x85 = (java.lang.Integer) x84.arg;
                class Fun86 extends f2j.Closure
                {
                  f2j.Closure x87 = this;
                  public void apply ()
                  {
                    final $Doc x88 = ($Doc) x87.arg;
                    $Doc x89;
                    switch (x88.tag)
                    {
                      case 2:
                        $Doc.$Doc$TEXT x90 = ($Doc.$Doc$TEXT) x88;
                        java.lang.String x91 = x90.field1;
                        $Doc x92 = x90.field2;
                        f2j.Closure x93 = x56;
                        x93.arg = x85;
                        x93.apply();
                        final f2j.Closure x94 = (f2j.Closure) x93.res;
                        f2j.Closure x95 = x94;
                        x95.arg = x92;
                        x95.apply();
                        final $Doc x96 = ($Doc) x95.res;
                        $Doc x97 = $doc.$Doc$TEXT(x91, x96);
                        x89 = x97;
                        break;
                      case 3:
                        $Doc.$Doc$LINE x98 = ($Doc.$Doc$LINE) x88;
                        java.lang.Integer x99 = x98.field1;
                        $Doc x100 = x98.field2;
                        final java.lang.Integer x101 = x85 + x99;
                        f2j.Closure x102 = x56;
                        x102.arg = x85;
                        x102.apply();
                        final f2j.Closure x103 = (f2j.Closure) x102.res;
                        f2j.Closure x104 = x103;
                        x104.arg = x100;
                        x104.apply();
                        final $Doc x105 = ($Doc) x104.res;
                        $Doc x106 = $doc.$Doc$LINE(x101, x105);
                        x89 = x106;
                        break;
                      case 1:
                        $Doc x107 = x88;
                        $Doc x108 = $doc.$Doc$Nil();
                        x89 = x108;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x89;
                  }
                }
                f2j.Closure x86 = new Fun86();
                res = x86;
              }
            }
            f2j.Closure x83 = new Fun83();
            x56 = x83;
            class Fun109 extends f2j.Closure
            {
              f2j.Closure x110 = this;
              public void apply ()
              {
                final java.lang.String x111 = (java.lang.String) x110.arg;
                $Doc x112 = $doc.$Doc$Nil();
                $Doc x113 = $doc.$Doc$TEXT(x111, x112);
                res = x113;
              }
            }
            f2j.Closure x109 = new Fun109();
            f2j.Closure x114 = x109;
            $Doc x115 = $doc.$Doc$Nil();
            $Doc x116 = $doc.$Doc$LINE(0, x115);
            $Doc x117 = x116;
            class $PList
            {
              final int tag;
              $PList (int tag)
              {
                this.tag = tag;
              }
              $PList $PList$Nill;
              $PList $PList$Nill ()
              {
                if ($PList$Nill == null)
                  $PList$Nill = new $PList(1);
                return $PList$Nill;
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
            class Let119
            {
              Object temp;
              f2j.Closure x121;
              f2j.Closure x122;
              f2j.Closure x123;
              {
                class Fun199 extends f2j.Closure
                {
                  f2j.Closure x200 = this;
                  public void apply ()
                  {
                    final $BTree x201 = ($BTree) x200.arg;
                    $Doc x202;
                    switch (x201.tag)
                    {
                      case 1:
                        $BTree.$BTree$Node x203 = ($BTree.$BTree$Node) x201;
                        java.lang.String x204 = x203.field1;
                        $PList x205 = x203.field2;
                        f2j.Closure x210 = x122;
                        x210.arg = x205;
                        x210.apply();
                        final $Doc x211 = ($Doc) x210.res;
                        f2j.Closure x206 = x114;
                        x206.arg = x204;
                        x206.apply();
                        final $Doc x207 = ($Doc) x206.res;
                        f2j.Closure x208 = x5;
                        x208.arg = x207;
                        x208.apply();
                        final f2j.Closure x209 = (f2j.Closure) x208.res;
                        f2j.Closure x212 = x209;
                        x212.arg = x211;
                        x212.apply();
                        final $Doc x213 = ($Doc) x212.res;
                        x202 = x213;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x202;
                  }
                }
                f2j.Closure x199 = new Fun199();
                x121 = x199;
                class Fun214 extends f2j.Closure
                {
                  f2j.Closure x215 = this;
                  public void apply ()
                  {
                    final $PList x216 = ($PList) x215.arg;
                    $Doc x217;
                    switch (x216.tag)
                    {
                      case 1:
                        $PList x218 = x216;
                        x217 = x2;
                        break;
                      case 2:
                        $PList.$PList$Cons x219 = ($PList.$PList$Cons) x216;
                        $BTree x220 = ($BTree) x219.field1;
                        $PList x221 = x219.field2;
                        f2j.Closure x244 = x114;
                        x244.arg = "]";
                        x244.apply();
                        final $Doc x245 = ($Doc) x244.res;
                        f2j.Closure x230 = x123;
                        x230.arg = x221;
                        x230.apply();
                        final $Doc x231 = ($Doc) x230.res;
                        f2j.Closure x228 = x5;
                        x228.arg = x117;
                        x228.apply();
                        final f2j.Closure x229 = (f2j.Closure) x228.res;
                        f2j.Closure x232 = x229;
                        x232.arg = x231;
                        x232.apply();
                        final $Doc x233 = ($Doc) x232.res;
                        f2j.Closure x226 = x56;
                        x226.arg = 2;
                        x226.apply();
                        final f2j.Closure x227 = (f2j.Closure) x226.res;
                        f2j.Closure x234 = x227;
                        x234.arg = x233;
                        x234.apply();
                        final $Doc x235 = ($Doc) x234.res;
                        f2j.Closure x222 = x114;
                        x222.arg = "[";
                        x222.apply();
                        final $Doc x223 = ($Doc) x222.res;
                        f2j.Closure x224 = x5;
                        x224.arg = x223;
                        x224.apply();
                        final f2j.Closure x225 = (f2j.Closure) x224.res;
                        f2j.Closure x236 = x225;
                        x236.arg = x235;
                        x236.apply();
                        final $Doc x237 = ($Doc) x236.res;
                        f2j.Closure x238 = x5;
                        x238.arg = x237;
                        x238.apply();
                        final f2j.Closure x239 = (f2j.Closure) x238.res;
                        f2j.Closure x240 = x239;
                        x240.arg = x117;
                        x240.apply();
                        final $Doc x241 = ($Doc) x240.res;
                        f2j.Closure x242 = x5;
                        x242.arg = x241;
                        x242.apply();
                        final f2j.Closure x243 = (f2j.Closure) x242.res;
                        f2j.Closure x246 = x243;
                        x246.arg = x245;
                        x246.apply();
                        final $Doc x247 = ($Doc) x246.res;
                        x217 = x247;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x217;
                  }
                }
                f2j.Closure x214 = new Fun214();
                x122 = x214;
                class Fun248 extends f2j.Closure
                {
                  f2j.Closure x249 = this;
                  public void apply ()
                  {
                    final $PList x250 = ($PList) x249.arg;
                    $Doc x251;
                    switch (x250.tag)
                    {
                      case 1:
                        $PList x252 = x250;
                        x251 = x2;
                        break;
                      case 2:
                        $PList.$PList$Cons x253 = ($PList.$PList$Cons) x250;
                        $BTree x254 = ($BTree) x253.field1;
                        $PList x255 = x253.field2;
                        f2j.Closure x270 = x123;
                        x270.arg = x255;
                        x270.apply();
                        final $Doc x271 = ($Doc) x270.res;
                        f2j.Closure x260 = x114;
                        x260.arg = ",";
                        x260.apply();
                        final $Doc x261 = ($Doc) x260.res;
                        f2j.Closure x256 = x121;
                        x256.arg = x254;
                        x256.apply();
                        final $Doc x257 = ($Doc) x256.res;
                        f2j.Closure x258 = x5;
                        x258.arg = x257;
                        x258.apply();
                        final f2j.Closure x259 = (f2j.Closure) x258.res;
                        f2j.Closure x262 = x259;
                        x262.arg = x261;
                        x262.apply();
                        final $Doc x263 = ($Doc) x262.res;
                        f2j.Closure x264 = x5;
                        x264.arg = x263;
                        x264.apply();
                        final f2j.Closure x265 = (f2j.Closure) x264.res;
                        f2j.Closure x266 = x265;
                        x266.arg = x117;
                        x266.apply();
                        final $Doc x267 = ($Doc) x266.res;
                        f2j.Closure x268 = x5;
                        x268.arg = x267;
                        x268.apply();
                        final f2j.Closure x269 = (f2j.Closure) x268.res;
                        f2j.Closure x272 = x269;
                        x272.arg = x271;
                        x272.apply();
                        final $Doc x273 = ($Doc) x272.res;
                        x251 = x273;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x251;
                  }
                }
                f2j.Closure x248 = new Fun248();
                x123 = x248;
                class Fun274 extends f2j.Closure
                {
                  f2j.Closure x275 = this;
                  public void apply ()
                  {
                    final java.lang.String x276 = (java.lang.String) x275.arg;
                    class Fun277 extends f2j.Closure
                    {
                      f2j.Closure x278 = this;
                      public void apply ()
                      {
                        final java.lang.String x279 = (java.lang.String) x278.arg;
                        final java.lang.String x280 = x276.<java.lang.String>concat(x279);
                        res = x280;
                      }
                    }
                    f2j.Closure x277 = new Fun277();
                    res = x277;
                  }
                }
                f2j.Closure x274 = new Fun274();
                f2j.Closure x281 = x274;
                class Let282
                {
                  Object temp;
                  f2j.Closure x284;
                  {
                    class Fun308 extends f2j.Closure
                    {
                      f2j.Closure x309 = this;
                      public void apply ()
                      {
                        final $Doc x310 = ($Doc) x309.arg;
                        java.lang.String x311;
                        switch (x310.tag)
                        {
                          case 2:
                            $Doc.$Doc$TEXT x312 = ($Doc.$Doc$TEXT) x310;
                            java.lang.String x313 = x312.field1;
                            $Doc x314 = x312.field2;
                            f2j.Closure x317 = x284;
                            x317.arg = x314;
                            x317.apply();
                            final java.lang.String x318 = (java.lang.String) x317.res;
                            f2j.Closure x315 = x281;
                            x315.arg = x313;
                            x315.apply();
                            final f2j.Closure x316 = (f2j.Closure) x315.res;
                            f2j.Closure x319 = x316;
                            x319.arg = x318;
                            x319.apply();
                            final java.lang.String x320 = (java.lang.String) x319.res;
                            x311 = x320;
                            break;
                          case 3:
                            $Doc.$Doc$LINE x321 = ($Doc.$Doc$LINE) x310;
                            java.lang.Integer x322 = x321.field1;
                            $Doc x323 = x321.field2;
                            f2j.Closure x326 = x284;
                            x326.arg = x323;
                            x326.apply();
                            final java.lang.String x327 = (java.lang.String) x326.res;
                            f2j.Closure x324 = x281;
                            x324.arg = "\n";
                            x324.apply();
                            final f2j.Closure x325 = (f2j.Closure) x324.res;
                            f2j.Closure x328 = x325;
                            x328.arg = x327;
                            x328.apply();
                            final java.lang.String x329 = (java.lang.String) x328.res;
                            x311 = x329;
                            break;
                          case 1:
                            $Doc x330 = x310;
                            x311 = "";
                            break;
                          default:
                            throw new RuntimeException("pattern match fail");
                        }
                        res = x311;
                      }
                    }
                    f2j.Closure x308 = new Fun308();
                    x284 = x308;
                    $PList x331 = $plist.$PList$Nill();
                    $BTree x332 = $btree.$BTree$Node("bbbbb", x331);
                    $PList x333 = $plist.$PList$Nill();
                    $BTree x334 = $btree.$BTree$Node("eee", x333);
                    $PList x335 = $plist.$PList$Nill();
                    $BTree x336 = $btree.$BTree$Node("ffff", x335);
                    $PList x337 = $plist.$PList$Nill();
                    $PList x338 = $plist.$PList$Cons(x336, x337);
                    $PList x339 = $plist.$PList$Cons(x334, x338);
                    $PList x340 = $plist.$PList$Cons(x332, x339);
                    $BTree x341 = $btree.$BTree$Node("aaa", x340);
                    $BTree x342 = x341;
                    f2j.Closure x343 = x121;
                    x343.arg = x342;
                    x343.apply();
                    final $Doc x344 = ($Doc) x343.res;
                    f2j.Closure x345 = x284;
                    x345.arg = x344;
                    x345.apply();
                    final java.lang.String x346 = (java.lang.String) x345.res;
                    temp = x346;
                  }
                }
                Let282 x282 = new Let282();
                final java.lang.String x283 = (java.lang.String) x282.temp;
                temp = x283;
              }
            }
            Let119 x119 = new Let119();
            final java.lang.String x120 = (java.lang.String) x119.temp;
            temp = x120;
          }
        }
        Let54 x54 = new Let54();
        final java.lang.String x55 = (java.lang.String) x54.temp;
        temp = x55;
      }
    }
    Let3 x3 = new Let3();
    final java.lang.String x4 = (java.lang.String) x3.temp;
    return x4;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}