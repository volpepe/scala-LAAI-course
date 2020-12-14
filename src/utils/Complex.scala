package utils

class Complex(val a: Double, val b: Double) {
    def +(that: Complex) = new Complex(this.a+that.a, this.b+that.b)
    def *(that:Complex) = new Complex(
        this.a*that.a-this.b*that.b, this.a*that.b+that.a*this.b
    )
    def abs(): Double = Math.sqrt(this.a*this.a + this.b*this.b)
}


