lazy val sharedSettings = Seq(
  name         := "sail",
  scalaVersion := "3.3.1"
)

val sail = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"     % "1.0.0-M8" % Test,
      "com.lihaoyi"   %%% "fastparse" % "3.0.2",
      "com.monovore"  %%% "decline"   % "2.4.1"
    )
  )
  .nativeSettings {
    import scala.scalanative.build._
    nativeConfig ~= { nc =>
      nc.withLTO(LTO.thin)
        .withGC(GC.none)
        .withMode(Mode.releaseFull)
    }
  }
