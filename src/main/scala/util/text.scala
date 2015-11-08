package util

/**
  * Created by pouya on 08/11/15.
  */
object text {
  def deaccent(s: String): String = java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
  def alphanum_(s: String, r: String): String = s.replaceAll("\\W", r)
}
