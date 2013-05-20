/*
  Test Go's type system for inheritence and polymorphism.
*/
package main

import "fmt"

type Args struct {
  id uint
  check bool
  who string
}

type Args2 struct {
  // has to be anonymous and not a pointer
  // it really is best to think of these as nested structs, the language just
  // has convinience of seeing through all the nesting. e.g instead of needing
  // to type: Args2.Args.Id() you just type Args2.Id().... however Args2 is a
  // distinct type from Args, *no subclass relation has been established*.
  // e.g a function taking a Args argument won't accept an Args2 in its place.

  // This is kind of strange in that you inherit Args methods and fields but you
  // don't establish a type relation... I guess you do inherit the same subtype
  // relations as Args already has though. So if Args is a subtype of B & C
  // (through interfaces) then Args2 is also a subtype of B & C but has no
  // relation to Args. Its like a new type that uses Args as a starting point
  // but otherwise is not related to Args.
  Args
  CAS uint64
}

// So make Args3 a copy of Args but with Name and overriding the Id method...
type Args3 struct {
  Args
  Name string
}

func (this *Args3) Id() uint {
  return 666
}

// interfaces are used for polymorphism.
// so Args & Args2 are distinct types still but Arg is a subtype of T and by
// inheriting from Args, Args2 is also a subtype of T... saying subtype is wrong
// in some ways as I don't think you can really have a hierachy. Its just flat.
// So field names can't be polymorphic though so need to use getter / setters
// still.
type T interface {
  Id() uint
  Check() bool
  Who() string
}

// implement T (note this implements T for Args2 as well!)
func (this *Args) Id() uint {
  return this.id
}

func (this *Args) Check() bool {
  return this.check
}

func (this *Args) Who() string {
  return this.who
}

func (this *Args) PolyFunc() {
  fmt.Println("Args is", this.Who())
}

func main() {
  a := &Args{0, true, "Args"}
  b := &Args2{Args{1, true, "Args2"}, 12}
  // Can compose
  c := &Args2{*a, 13}
  d := &Args3{Args{2, true, "Args3"}, "Args3"}

  // DOESN'T COMPILE -- NOT SUBCLASSS
  // var d Args
  // d = *b
  
  // BUT ARGS2 inherits ARGS interfaces...
  fmt.Printf("a.Id() = %d\n", T.Id(a))
  fmt.Printf("b.Id() = %d\n", T.Id(b))

  // Wow -- can do selective overriding of Id for subclass.
  fmt.Printf("d.Id() = %d\n", T.Id(d))

  showArgs(a)
  // DOENS'T COMPILE
  // showArgs(b)
  // Can access anonymous struct by using its type
  // here the nesting is apparent!
  showArgs(&b.Args)
  showArgs(&c.Args)

  a.PolyFunc()
  // BUT, b has Args methods?
  b.PolyFunc()

  idT(a)
  idT(b)
  idT(c)
}

// Argument is Args, so not polymorphic sadly, Args2 won't be accepted
func showArgs(arg *Args) {
  fmt.Println("Args:", arg)
}

// Note argument is T! so polymorphic...!
func idT(t T) {
  fmt.Println("ID:", t.Id())
}

