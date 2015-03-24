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
            class Fun120 extends f2j.Closure
            {
              f2j.Closure x121 = this;
              public void apply ()
              {
                final Object x122 = x121.arg;
                class Fun123 extends f2j.Closure
                {
                  f2j.Closure x124 = this;
                  public void apply ()
                  {
                    final $PList x125 = ($PList) x124.arg;
                    $PList x126 = $plist.$PList$Cons(x122, x125);
                    res = x126;
                  }
                }
                f2j.Closure x123 = new Fun123();
                res = x123;
              }
            }
            f2j.Closure x120 = new Fun120();
            f2j.Closure x127 = x120;
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
            class Let128
            {
              Object temp;
              f2j.Closure x130;
              f2j.Closure x131;
              f2j.Closure x132;
              {
                class Fun225 extends f2j.Closure
                {
                  f2j.Closure x226 = this;
                  public void apply ()
                  {
                    final $BTree x227 = ($BTree) x226.arg;
                    $Doc x228;
                    switch (x227.tag)
                    {
                      case 1:
                        $BTree.$BTree$Node x229 = ($BTree.$BTree$Node) x227;
                        java.lang.String x230 = x229.field1;
                        $PList x231 = x229.field2;
                        f2j.Closure x236 = x131;
                        x236.arg = x231;
                        x236.apply();
                        final $Doc x237 = ($Doc) x236.res;
                        f2j.Closure x232 = x114;
                        x232.arg = x230;
                        x232.apply();
                        final $Doc x233 = ($Doc) x232.res;
                        f2j.Closure x234 = x5;
                        x234.arg = x233;
                        x234.apply();
                        final f2j.Closure x235 = (f2j.Closure) x234.res;
                        f2j.Closure x238 = x235;
                        x238.arg = x237;
                        x238.apply();
                        final $Doc x239 = ($Doc) x238.res;
                        x228 = x239;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x228;
                  }
                }
                f2j.Closure x225 = new Fun225();
                x130 = x225;
                class Fun240 extends f2j.Closure
                {
                  f2j.Closure x241 = this;
                  public void apply ()
                  {
                    final $PList x242 = ($PList) x241.arg;
                    $Doc x243;
                    switch (x242.tag)
                    {
                      case 1:
                        $PList x244 = x242;
                        x243 = x2;
                        break;
                      case 2:
                        $PList.$PList$Cons x245 = ($PList.$PList$Cons) x242;
                        $BTree x246 = ($BTree) x245.field1;
                        $PList x247 = x245.field2;
                        f2j.Closure x275 = x114;
                        x275.arg = "]";
                        x275.apply();
                        final $Doc x276 = ($Doc) x275.res;
                        f2j.Closure x257 = x127;
                        x257.arg = x246;
                        x257.apply();
                        final f2j.Closure x258 = (f2j.Closure) x257.res;
                        f2j.Closure x259 = x258;
                        x259.arg = x247;
                        x259.apply();
                        final $PList x260 = ($PList) x259.res;
                        f2j.Closure x261 = x132;
                        x261.arg = x260;
                        x261.apply();
                        final $Doc x262 = ($Doc) x261.res;
                        f2j.Closure x254 = x5;
                        x254.arg = x117;
                        x254.apply();
                        final f2j.Closure x255 = (f2j.Closure) x254.res;
                        f2j.Closure x263 = x255;
                        x263.arg = x262;
                        x263.apply();
                        final $Doc x264 = ($Doc) x263.res;
                        f2j.Closure x252 = x56;
                        x252.arg = 2;
                        x252.apply();
                        final f2j.Closure x253 = (f2j.Closure) x252.res;
                        f2j.Closure x265 = x253;
                        x265.arg = x264;
                        x265.apply();
                        final $Doc x266 = ($Doc) x265.res;
                        f2j.Closure x248 = x114;
                        x248.arg = "[";
                        x248.apply();
                        final $Doc x249 = ($Doc) x248.res;
                        f2j.Closure x250 = x5;
                        x250.arg = x249;
                        x250.apply();
                        final f2j.Closure x251 = (f2j.Closure) x250.res;
                        f2j.Closure x267 = x251;
                        x267.arg = x266;
                        x267.apply();
                        final $Doc x268 = ($Doc) x267.res;
                        f2j.Closure x269 = x5;
                        x269.arg = x268;
                        x269.apply();
                        final f2j.Closure x270 = (f2j.Closure) x269.res;
                        f2j.Closure x271 = x270;
                        x271.arg = x117;
                        x271.apply();
                        final $Doc x272 = ($Doc) x271.res;
                        f2j.Closure x273 = x5;
                        x273.arg = x272;
                        x273.apply();
                        final f2j.Closure x274 = (f2j.Closure) x273.res;
                        f2j.Closure x277 = x274;
                        x277.arg = x276;
                        x277.apply();
                        final $Doc x278 = ($Doc) x277.res;
                        x243 = x278;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x243;
                  }
                }
                f2j.Closure x240 = new Fun240();
                x131 = x240;
                class Fun279 extends f2j.Closure
                {
                  f2j.Closure x280 = this;
                  public void apply ()
                  {
                    final $PList x281 = ($PList) x280.arg;
                    $Doc x282;
                    switch (x281.tag)
                    {
                      case 1:
                        $PList x283 = x281;
                        x282 = x2;
                        break;
                      case 2:
                        $PList.$PList$Cons x284 = ($PList.$PList$Cons) x281;
                        $BTree x285 = ($BTree) x284.field1;
                        $PList x286 = x284.field2;
                        $Doc x287;
                        switch (x286.tag)
                        {
                          case 1:
                            $PList x288 = x286;
                            f2j.Closure x289 = x130;
                            x289.arg = x285;
                            x289.apply();
                            final $Doc x290 = ($Doc) x289.res;
                            x287 = x290;
                            break;
                          case 2:
                            $PList.$PList$Cons x291 = ($PList.$PList$Cons) x286;
                            $BTree x292 = ($BTree) x291.field1;
                            $PList x293 = x291.field2;
                            f2j.Closure x309 = x127;
                            x309.arg = x292;
                            x309.apply();
                            final f2j.Closure x310 = (f2j.Closure) x309.res;
                            f2j.Closure x311 = x310;
                            x311.arg = x293;
                            x311.apply();
                            final $PList x312 = ($PList) x311.res;
                            f2j.Closure x313 = x132;
                            x313.arg = x312;
                            x313.apply();
                            final $Doc x314 = ($Doc) x313.res;
                            f2j.Closure x298 = x114;
                            x298.arg = ",";
                            x298.apply();
                            final $Doc x299 = ($Doc) x298.res;
                            f2j.Closure x294 = x130;
                            x294.arg = x285;
                            x294.apply();
                            final $Doc x295 = ($Doc) x294.res;
                            f2j.Closure x296 = x5;
                            x296.arg = x295;
                            x296.apply();
                            final f2j.Closure x297 = (f2j.Closure) x296.res;
                            f2j.Closure x300 = x297;
                            x300.arg = x299;
                            x300.apply();
                            final $Doc x301 = ($Doc) x300.res;
                            f2j.Closure x302 = x5;
                            x302.arg = x301;
                            x302.apply();
                            final f2j.Closure x303 = (f2j.Closure) x302.res;
                            f2j.Closure x304 = x303;
                            x304.arg = x117;
                            x304.apply();
                            final $Doc x305 = ($Doc) x304.res;
                            f2j.Closure x306 = x5;
                            x306.arg = x305;
                            x306.apply();
                            final f2j.Closure x307 = (f2j.Closure) x306.res;
                            f2j.Closure x315 = x307;
                            x315.arg = x314;
                            x315.apply();
                            final $Doc x316 = ($Doc) x315.res;
                            x287 = x316;
                            break;
                          default:
                            throw new RuntimeException("pattern match fail");
                        }
                        x282 = x287;
                        break;
                      default:
                        throw new RuntimeException("pattern match fail");
                    }
                    res = x282;
                  }
                }
                f2j.Closure x279 = new Fun279();
                x132 = x279;
                class Fun317 extends f2j.Closure
                {
                  f2j.Closure x318 = this;
                  public void apply ()
                  {
                    final java.lang.String x319 = (java.lang.String) x318.arg;
                    class Fun320 extends f2j.Closure
                    {
                      f2j.Closure x321 = this;
                      public void apply ()
                      {
                        final java.lang.String x322 = (java.lang.String) x321.arg;
                        final java.lang.String x323 = x319.<java.lang.String>concat(x322);
                        res = x323;
                      }
                    }
                    f2j.Closure x320 = new Fun320();
                    res = x320;
                  }
                }
                f2j.Closure x317 = new Fun317();
                f2j.Closure x324 = x317;
                class Let325
                {
                  Object temp;
                  f2j.Closure x327;
                  {
                    class Fun337 extends f2j.Closure
                    {
                      f2j.Closure x338 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x339 = (java.lang.Integer) x338.arg;
                        final java.lang.Boolean x341 = x339 == 0;
                        java.lang.String ifres340;
                        if (x341)
                        {
                          ifres340 = "";
                        }
                        else
                        {
                          final java.lang.Integer x342 = x339 - 1;
                          f2j.Closure x343 = x327;
                          x343.arg = x342;
                          x343.apply();
                          final java.lang.String x344 = (java.lang.String) x343.res;
                          final java.lang.String x345 = " ".<java.lang.String>concat(x344);
                          ifres340 = x345;
                        }
                        res = ifres340;
                      }
                    }
                    f2j.Closure x337 = new Fun337();
                    x327 = x337;
                    class Let346
                    {
                      Object temp;
                      f2j.Closure x348;
                      {
                        class Fun378 extends f2j.Closure
                        {
                          f2j.Closure x379 = this;
                          public void apply ()
                          {
                            final $Doc x380 = ($Doc) x379.arg;
                            java.lang.String x381;
                            switch (x380.tag)
                            {
                              case 2:
                                $Doc.$Doc$TEXT x382 = ($Doc.$Doc$TEXT) x380;
                                java.lang.String x383 = x382.field1;
                                $Doc x384 = x382.field2;
                                f2j.Closure x387 = x348;
                                x387.arg = x384;
                                x387.apply();
                                final java.lang.String x388 = (java.lang.String) x387.res;
                                f2j.Closure x385 = x324;
                                x385.arg = x383;
                                x385.apply();
                                final f2j.Closure x386 = (f2j.Closure) x385.res;
                                f2j.Closure x389 = x386;
                                x389.arg = x388;
                                x389.apply();
                                final java.lang.String x390 = (java.lang.String) x389.res;
                                x381 = x390;
                                break;
                              case 3:
                                $Doc.$Doc$LINE x391 = ($Doc.$Doc$LINE) x380;
                                java.lang.Integer x392 = x391.field1;
                                $Doc x393 = x391.field2;
                                f2j.Closure x402 = x348;
                                x402.arg = x393;
                                x402.apply();
                                final java.lang.String x403 = (java.lang.String) x402.res;
                                f2j.Closure x396 = x327;
                                x396.arg = x392;
                                x396.apply();
                                final java.lang.String x397 = (java.lang.String) x396.res;
                                f2j.Closure x394 = x324;
                                x394.arg = "\n";
                                x394.apply();
                                final f2j.Closure x395 = (f2j.Closure) x394.res;
                                f2j.Closure x398 = x395;
                                x398.arg = x397;
                                x398.apply();
                                final java.lang.String x399 = (java.lang.String) x398.res;
                                f2j.Closure x400 = x324;
                                x400.arg = x399;
                                x400.apply();
                                final f2j.Closure x401 = (f2j.Closure) x400.res;
                                f2j.Closure x404 = x401;
                                x404.arg = x403;
                                x404.apply();
                                final java.lang.String x405 = (java.lang.String) x404.res;
                                x381 = x405;
                                break;
                              case 1:
                                $Doc x406 = x380;
                                x381 = "";
                                break;
                              default:
                                throw new RuntimeException("pattern match fail");
                            }
                            res = x381;
                          }
                        }
                        f2j.Closure x378 = new Fun378();
                        x348 = x378;
                        $PList x407 = $plist.$PList$Nill();
                        $BTree x408 = $btree.$BTree$Node("bbbbb", x407);
                        $PList x409 = $plist.$PList$Nill();
                        $BTree x410 = $btree.$BTree$Node("eee", x409);
                        $PList x411 = $plist.$PList$Nill();
                        $BTree x412 = $btree.$BTree$Node("ffff", x411);
                        $PList x413 = $plist.$PList$Nill();
                        $PList x414 = $plist.$PList$Cons(x412, x413);
                        $PList x415 = $plist.$PList$Cons(x410, x414);
                        $PList x416 = $plist.$PList$Cons(x408, x415);
                        $BTree x417 = $btree.$BTree$Node("aaa", x416);
                        $BTree x418 = x417;
                        $PList x419 = $plist.$PList$Nill();
                        $BTree x420 = $btree.$BTree$Node("ee", x419);
                        $PList x421 = $plist.$PList$Nill();
                        $BTree x422 = $btree.$BTree$Node("ff", x421);
                        $PList x423 = $plist.$PList$Nill();
                        $PList x424 = $plist.$PList$Cons(x422, x423);
                        $PList x425 = $plist.$PList$Cons(x420, x424);
                        $BTree x426 = $btree.$BTree$Node("bbb", x425);
                        $PList x427 = $plist.$PList$Nill();
                        $BTree x428 = $btree.$BTree$Node("cc", x427);
                        $PList x429 = $plist.$PList$Nill();
                        $BTree x430 = $btree.$BTree$Node("dd", x429);
                        $PList x431 = $plist.$PList$Nill();
                        $PList x432 = $plist.$PList$Cons(x430, x431);
                        $PList x433 = $plist.$PList$Cons(x428, x432);
                        $PList x434 = $plist.$PList$Cons(x426, x433);
                        $BTree x435 = $btree.$BTree$Node("aaa", x434);
                        $BTree x436 = x435;
                        f2j.Closure x437 = x130;
                        x437.arg = x436;
                        x437.apply();
                        final $Doc x438 = ($Doc) x437.res;
                        f2j.Closure x439 = x348;
                        x439.arg = x438;
                        x439.apply();
                        final java.lang.String x440 = (java.lang.String) x439.res;
                        temp = x440;
                      }
                    }
                    Let346 x346 = new Let346();
                    final java.lang.String x347 = (java.lang.String) x346.temp;
                    temp = x347;
                  }
                }
                Let325 x325 = new Let325();
                final java.lang.String x326 = (java.lang.String) x325.temp;
                temp = x326;
              }
            }
            Let128 x128 = new Let128();
            final java.lang.String x129 = (java.lang.String) x128.temp;
            temp = x129;
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