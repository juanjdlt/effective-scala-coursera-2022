case class Card(shape: Shape, number: Number, color: Color, shading: Shading)

sealed trait Property
sealed trait Shape extends Property
sealed trait Number extends Property
sealed trait Color extends Property
sealed trait Shading extends Property

case object Diamond extends Shape 
case object Squiggle extends Shape
case object Oval extends Shape

case object One extends Number
case object Two extends Number
case object Three extends Number

case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Purple extends Color

case object Open extends Shading
case object Striped extends Shading
case object Solid extends Shading

// enum Property:
//     case Shape, Color, Shading, Number

// enum Shape:
//   case Diamond, Squiggle, Oval 

// enum Color:
//   case Red, Green, Purple

// enum Shading:
//   case Open, Striped, Solid

// enum Number:
//   case One, Two, Three

// Let's create a deck of cards. Note that we are using Fully Qualified Names
// (FQN) to access the `enum` members. Later in the course, we will use one of
// the Scala 3 features that will allow us to avoid having to use FQNs
val deck = List(
  Card(Diamond,  One, Purple, Striped),
  Card(Squiggle, Two, Red,    Open),
  Card(Oval,     Three, Green,  Solid)
)

def isValidSet(card1: Card, card2: Card, card3: Card): Boolean =
  checkShapeProperty(card1, card2, card3)   &&
  checkNumberProperty(card1, card2, card3)  &&
  checkColorProperty(card1, card2, card3)   &&
  checkShadingProperty(card1, card2, card3)

def isValidSet2(card1: Card, card2: Card, card3: Card): Boolean =
  checkProperty(card1.color, card2.color, card3.color)   &&
  checkProperty(card1.number, card2.number, card3.number)  &&
  checkProperty(card1.shading, card2.shading, card3.shading)   &&
  checkProperty(card1.shape, card2.shape, card3.shape)


def checkProperty(card1Property: Property, card2Property: Property, card3Property: Property) =
  def allSame =
    card1Property == card2Property && card1Property == card3Property
  def allDifferent =
    card1Property != card2Property &&
    card1Property != card3Property &&
    card2Property != card3Property
  allSame || allDifferent

def checkShapeProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.shape == card2.shape && card1.shape == card3.shape
  def allDifferent =
    card1.shape != card2.shape &&
    card1.shape != card3.shape &&
    card2.shape != card3.shape
  allSame || allDifferent

def checkNumberProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.number == card2.number && card1.number == card3.number
  def allDifferent =
    card1.number != card2.number &&
    card1.number != card3.number &&
    card2.number != card3.number
  allSame || allDifferent

def checkColorProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.color == card2.color && card1.color == card3.color
  def allDifferent =
    card1.color != card2.color &&
    card1.color != card3.color &&
    card2.color != card3.color
  allSame || allDifferent

def checkShadingProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.shading == card2.shading && card1.shading == card3.shading
  def allDifferent =
    card1.shading != card2.shading &&
    card1.shading != card3.shading &&
    card2.shading != card3.shading
  allSame || allDifferent

isValidSet(
  Card(Diamond,  One,   Purple, Striped),
  Card(Squiggle, Two,   Purple, Open),
  Card(Oval,     Three, Purple, Solid)
)

isValidSet(
  Card(Diamond,  Two,   Purple, Striped),
  Card(Squiggle, Two,   Purple, Open),
  Card(Oval,     Three, Purple, Solid)
)