class Rational(n:Int, d: Int){
  require(d != 0)
  
  private val g = gcd(n.abs, d.abs)
  
  val numer: Int = n /g
  val denom: Int = d /g
  
  def this(n: Int) = this(n, 1)
  
  override def toString = numer + "/" + denom
  
  private def gcd(a: Int, b: Int): Int = 
    if(b == 0) a else gcd(b, a % b)
  
  def + (that: Rational): Rational = 
    new Rational(
	  numer * that.denom + that.numer * denom,
	  denom * that.denom
	 )
  
  def * (that : Rational): Rational = 
    new Rational(
	  this.numer * that.numer,
	  this.denom * that.denom
	)
  
  def lessThan(that: Rational)=
    this.numer * that.denom < that.numer * this.denom
	
  def max(that: Rational)=
    if (lessThan(that)) that else this
 }
  