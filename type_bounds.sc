

trait Thing
class Vehicle extends Thing

class Car extends Vehicle
class Jeep extends Car
class Coupe extends Car

class Motorcycle extends Vehicle

class Vegetable

class Parking[A](val place: A)
val park1 = new Parking[Motorcycle](new Motorcycle)
// val park2 = new Parking[Motorcycle](new Car)     <- type mismatch
val park3 = new Parking[Car](new Jeep)

/* without explicitely specifying type but let scala infer (to Vehicle) */
class Parking1[A](val place1: A, val place2: A)
val park4 =  new Parking1(new Car, new Motorcycle)

/* more restrictive type parameters for Parking class using upper bound */
class Parking2[A <: Vehicle](val place: A)
val park5 = new Parking2(new Jeep)
// val park6 = new Parking2(new Vegetable)          <- type mismatch
val park6 = new Parking2(new Vehicle)

/* more restrictive type parameters for Parking class using lower bound */
class Parking3[A >: Jeep](val place: A)
val park7 = new Parking3(new Car)
val park8 = new Parking3(new Jeep)

class Bicycle extends Vehicle
class Tricycle extends Bicycle

class Parking4[A >: Bicycle <: Vehicle](val plaza: A)
// val park9 = new Parking4(new AnyRef)             <- type mismatch
val park10 = new Parking4(new Motorcycle)
val park11 = new Parking4(new Car)
val park12 = new Parking4(new Tricycle)

//  class Parking5[+A] {
//   def parkIt(element: A): Parking[A] = new Parking(element)  <-- type mismatch
//  }

class Parking5[+A](val element: A)
{
    def parkIt[B >: A](element: B): Parking5[B] = new Parking5(element)
}
