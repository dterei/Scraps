public class T {
  static class A {
      public int x = 3;
      public int f() {
          return x;
      }
      public int g() {
          return x + f();
      }
  }

  static class B extends A {
      public int x = 30;
      public int f() {
          return x;
      }
  }

  static class C extends B {
      public int x = 300;
      public int g() {
          return x + f();
      }
  }

  static class D extends C {
  }

  public static void main(String[] args) {
    C c = new C();
    B b = c;
    A a = c;

    // field name resolution starts from the variable type! they aren't virtual
    // (in some sense).
    System.out.println(c.x); // 300
    System.out.println(b.x); // 30
    System.out.println(a.x); // 3

    D d = new D();
    System.out.println(d.x); // 3
    System.out.println(((C)(d)).x); // 3


    System.out.println(a.x + a.f()); // 33
    System.out.println(a.g()); // 330
  }
}

