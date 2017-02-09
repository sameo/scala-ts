package com.mpc.scalats

import java.sql.Timestamp

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptGenerator

/**
  * Created by Milosz on 06.12.2016.
  */

case class Foo[T, Q](a : T, b: List[Q])

case class Bar(b: Foo[String, String], c:Set[Foo[Int, Timestamp]])

case class Xyz(bars: Option[List[Option[Bar]]])

object GenericsExample {

  def main(args: Array[String]) {
    TypeScriptGenerator.generateFromClassNames(List("com.mpc.scalats.Xyz"))(Config())
  }

}
