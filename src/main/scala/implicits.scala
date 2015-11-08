package implicits

object defaults {

  type  Sca = Option[Double]
  type  Cat = Option[String]

  val nilSca = none[Double]
  val nilCat = none[String]
}

object Sca_NaNisNone {
  import implicits.defaults.{Sca,nilSca}

  implicit val zeroSca = () => nilSca

  implicit val canonSca = () => (o: Sca) => o match {
    case None => nilSca
    case Some(d) if d.isNaN => nilSca
    case Some(d) => o
  }
}

object Cat_Trim {
  import implicits.defaults.{Cat,nilCat}
  import util.text.{deaccent,alphanum_}

  implicit val zeroCat = () => nilCat

  implicit val canonCat = () => (o: Cat) => o match {
    case None => nilCat
    case Some(s) if s.trim.isEmpty => nilCat
    case Some(s) => o
  }
}

object Cat_TrimDeaccent {
  import implicits.defaults.{Cat,nilCat}
  import util.text.{deaccent,alphanum_}

  implicit val zeroCat = () => nilCat

  implicit val canonCat = () => (o: Cat) => o match {
    case None => nilCat
    case Some(s) if s.trim.isEmpty => nilCat
    case Some(s) => {
      val ds = deaccent(s.trim)
      if (ds.isEmpty) nilCat else Some(ds)
    }
  }
}

object Cat_TrimDeaccentTolower {
  import implicits.defaults.{Cat,nilCat}
  import util.text.{deaccent,alphanum_}

  implicit val zeroCat = () => nilCat

  implicit val canonCat = () => (o: Cat) => o match {
    case None => nilCat
    case Some(s) if s.trim.isEmpty => nilCat
    case Some(s) => {
      val ds = deaccent(s.trim).toLowerCase
      if (ds.isEmpty) nilCat else Some(ds)
    }
  }
}

object Cat_TrimDeaccentTolowerAlphanum {
  import implicits.defaults.{Cat,nilCat}
  import util.text.{deaccent,alphanum_}

  implicit val zeroCat = () => nilCat

  implicit val canonCat = () => (o: Cat) => o match {
    case None => nilCat
    case Some(s) => {
      val ds = alphanum_(deaccent(s.trim).toLowerCase,"")
      if (ds.isEmpty) nilCat else Some(ds)
    }
  }
}

object Cat_TrimDeaccentTolowerAlphanum_ {
  import implicits.defaults.{Cat,nilCat}
  import util.text.{deaccent,alphanum_}

  implicit val zeroCat = () => nilCat

  implicit val canonCat = () => (o: Cat) => o match {
    case None => nilCat
    case Some(s) => {
      val ds = alphanum_(deaccent(s.trim).toLowerCase,"_")
      if (ds.isEmpty) nilCat else Some(ds)
    }
  }
}
