class Rational(n: Int, d: Int) {
    """
    Class which represents and performs operations on 
    rational numbers (represented as fractions of integers).

    To perform Rational operations on Ints ( Ex: 2 * new Rational(5,2) ),
    add an implicit conversion:
        
        implicit def intToRationals(x: Int) = new Rational(x)

    This will convert Ints to Rationals automatically in a number of situations.
    """
    require(d != 0)

    private val g = gcd(n.abs, d.abs)
    val numer = n / g
    val denom = d / g

    def this(n: Int) = this(n,1) // Auxiliary constructor

    override def toString = numer +"/"+ denom

    private def gcd(a: Int, b: Int): Int =
        if(b==0) a else gcd(b, a % b)

    def inv: Rational = new Rational(this.denom, this.numer)

    def unary_- : Rational = new Rational(-this.numer, this.denom)

    def +(that: Rational): Rational = 
        new Rational(this.denom * that.numer + this.numer*that.denom, 
                     this.denom*that.denom)

    def +(that: Int): Rational = 
        this + new Rational(that)

    def -(that: Rational): Rational =
        this + (-that)

    def -(that: Int): Rational = 
        this - new Rational(that)

    def *(that: Rational): Rational = 
        new Rational(this.numer*that.numer, this.denom*that.denom)

    def *(that: Int): Rational = 
        this * new Rational(that)

    def /(that:Rational): Rational = 
        this * that.inv

    def /(that: Int): Rational = 
        this / new Rational(that)

    def <(that: Rational): Boolean = 
        this.numer * that.denom < that.numer * this.denom

    def <(that: Int): Boolean =
        this < new Rational(that)

    def >(that: Rational): Boolean = 
        this.numer * that.denom > that.numer * this.denom

    def >(that: Int): Boolean =
        this > new Rational(that)

    def <=(that: Rational): Boolean = !(this > that)

    def <=(that: Int): Boolean = this <= new Rational(that)

    def >=(that: Rational): Boolean = !(this < that)

    def >=(that: Int): Boolean = this >= new Rational(that)

    def max(that:Rational) = 
        if (this < that) that else this
}
