enablePlugins(ScalaNativePlugin)
import scala.scalanative.build._

nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.none)
}

val sail = (project in file("."))
  .settings(
    name         := "sail",
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"     % "1.0.0-M8" % Test,
      "com.lihaoyi"   %%% "fastparse" % "3.0.2",
      "com.monovore"  %%% "decline"   % "2.4.1"
    )
  )
