organization := "plutarch"
name := "plutarch"
organizationName := "Plutarch"

version := "0.1"

lazy val scalaV = "2.11.8"
lazy val akkaV = "2.5.21"
lazy val akkaHttpV = "10.1.8"

lazy val boopickleV = "1.3.0"
lazy val upickleV = "0.4.4"
lazy val scalatagsV = "0.6.7"
lazy val sparkVer = "2.3.4"

lazy val scalacssV = "0.5.5"
lazy val faV = "5.0.10"
lazy val w3cssV = "4.1.0"

lazy val fullOpt = true

lazy val root = project.in(file(".")).aggregate(frontend, backend)

lazy val stylesheets = Seq(
  s"w3-css/$w3cssV/w3.css",
  s"font-awesome/$faV/web-fonts-with-css/css/fontawesome-all.min.css")

lazy val backend =
  project.in(file("backend"))
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings)
    .settings(scalariformSettings)
    .settings(
        buildInfoKeys := Seq[BuildInfoKey](
          name,
          version,
          scalaVersion,
          sbtVersion,
          "fullOpt" -> fullOpt,
          "stylesheets" -> stylesheets
        ),
        buildInfoPackage := "buildinfo"
    )
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % akkaV,
        "com.typesafe.akka" %% "akka-stream" % akkaV,
        "com.typesafe.akka" %% "akka-http" % akkaHttpV,
        "com.typesafe.akka" %% "akka-slf4j" % akkaV,
        "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,

        "org.webjars" % "webjars-locator" % "0.36",
        "org.webjars.npm" % "w3-css" % w3cssV,
        "org.webjars" % "font-awesome" % "5.0.10",

        "io.suzaku" %% "boopickle" % boopickleV,
        "com.lihaoyi" %% "upickle" % upickleV,
        "com.lihaoyi" %% "scalatags" % scalatagsV,

        "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
        "org.slf4j" % "slf4j-api" % "1.7.25",
        "ch.qos.logback" % "logback-classic" % "1.2.3",
        "com.typesafe" % "config" % "1.3.2",

        "org.apache.spark"  %% "spark-core" % sparkVer % Provided,
        "org.apache.spark"  %% "spark-sql" % sparkVer % Provided,

        "com.typesafe.akka" %% "akka-testkit" % akkaV,
        "org.scalactic" %% "scalactic" % "3.0.3",
        "org.scalatest" %% "scalatest" % "3.0.3" % "test",
        "commons-io" % "commons-io" % "2.6" % "test",

        // misc
        "com.google.cloud" % "google-cloud-pubsub" % "1.102.0",
        "com.google.api" % "gax-grpc" % "1.52.0"

        //"org.twitter4j" % "twitter4j-stream" % "4.0.7",
        //"org.json4s" %% "json4s-jackson" % "3.6.5",
      ),
      resourceGenerators in Compile += Def.task {
        val f1 = (jsOpts in Compile in frontend).value.data
        val f1SourceMap = f1.getParentFile / (f1.getName + ".map")
        val f2 = (packageJSDependencies in Compile in frontend).value
        val f3 = (packageMinifiedJSDependencies in Compile in frontend).value
        val files = Seq(f1, f1SourceMap, f2, f3)
        //val files = ((crossTarget in(frontend, Compile)).value ** ("*.js" || "*.map")).get
        val mappings: Seq[(File,String)] = files pair Path.rebase(
          (crossTarget in(frontend, Compile)).value,
          ((resourceManaged in  Compile).value / "web" / "js").getAbsolutePath
        )
        val map: Seq[(File, File)] = mappings.map { case (s, t) => (s, file(t))}
        IO.copy(map).toSeq
      },
      watchSources ++= (watchSources in frontend).value,
      assemblyJarName in assembly := "backend.jar",
      assemblyMergeStrategy in assembly := {
        //case PathList("application.conf") => MergeStrategy.discard
        case PathList("logback.xml") => MergeStrategy.discard
        case x =>
          val oldStrategy = (assemblyMergeStrategy in assembly).value
          oldStrategy(x)
      },
      // to make it work with spark
      assemblyShadeRules in assembly := Seq(
        ShadeRule.rename("com.fasterxml.**" -> "shaded.fasterxml.@1").inAll,
        ShadeRule.rename("com.google.common.**" -> "repackaged.com.google.common.@1").inAll,
        ShadeRule.rename("com.google.protobuf.**" -> "repackaged.com.google.protobuf.@1").inAll
      ),
      fork in run := true,
      fork in Test := true
    )
    .dependsOn(sharedJvm)

lazy val frontend =
  project.in(file("frontend"))
    .enablePlugins(ScalaJSPlugin)
    .settings(commonSettings)
    .settings(scalariformSettings)
    .settings(
      if (fullOpt) scalaJSStage in Global := FullOptStage else Nil,
      scalaJSUseMainModuleInitializer := true,
      scalaJSUseMainModuleInitializer in Test := false,
      skip in packageJSDependencies := false,
      testFrameworks += new TestFramework("utest.runner.Framework"),
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalacss" %%% "ext-scalatags" % scalacssV,
        "com.github.japgolly.scalacss" %%% "core" % scalacssV,
        "io.suzaku" %%% "boopickle" % boopickleV,
        "com.lihaoyi" %%% "scalatags" % scalatagsV,
        "com.lihaoyi" %%% "upickle" % upickleV,
        "com.lihaoyi" %%% "scalarx" % "0.4.0",
        "org.scala-js" %%% "scalajs-dom" % "0.9.5",
        "biz.enef" %%% "slogging" % "0.6.1"
      )
    )
    .dependsOn(sharedJs)

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared"))
  .settings(scalariformSettings)
  .settings(
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "io.suzaku" %% "boopickle" % boopickleV
    )
  )

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

import scalariform.formatter.preferences._
lazy val scalariformSettings = scalariformPreferences := scalariformPreferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)

def jsOpts = if (fullOpt) fullOptJS else fastOptJS

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := scalaV,
  javaOptions ++= Seq("-Xmx8G", "-Xms8G", "-XX:+UseG1GC")
  //, "-Xloggc:gclog.log", "-XX:+PrintGCCause", "-XX:+PrintGCDetails", "-XX:+PrintGCTimeStamps", "-XX:+PrintTenuringDistribution")
)