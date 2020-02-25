package scodec.bits

import hedgehog._
import hedgehog.core.{Failed, GaveUp, OK, PropertyConfig, PropertyT, Seed}
import hedgehog.runner.Test

import munit.{Assertions, FunSuite, Location, TestOptions}

trait PropertySuite extends FunSuite {

  lazy val propertyConfig: PropertyConfig = PropertyConfig.default
  lazy val propertySeed: Long = System.currentTimeMillis()

  def property(
      options: TestOptions,
      config: PropertyConfig = propertyConfig,
      seed: Long = propertySeed
  )(prop: PropertyT[Result])(implicit loc: Location): Unit =
    test(options)(check(prop, config, seed))

  def check(
      prop: PropertyT[Result],
      config: PropertyConfig = propertyConfig,
      seed: Long = propertySeed
  )(implicit loc: Location): Unit = {
    val report = Property.check(config, prop, Seed.fromLong(seed))
    val coverage = Test.renderCoverage(report.coverage, report.tests)
    report.status match {
      case Failed(shrinks, log) =>
        Assertions.fail(
          render(
            s"Falsified after ${report.tests.value} passed tests and ${shrinks.value} shrinks using seed $seed",
            log.map(Test.renderLog) ++ coverage
          )
        )
      case GaveUp =>
        Assertions.fail(
          render(
            s"Gave up after ${report.tests.value} passed tests using seed value $seed. ${report.discards.value} were discarded",
            coverage
          )
        )
      case OK =>
        ()
    }
  }

  private def render(msg: String, extras: List[String]): String =
    (("- " + msg) :: extras.map(e => "> " + e)).mkString("\n")

  implicit def unitToResult(u: Unit): Result = {
    val _ = u
    Result.success
  }
}
