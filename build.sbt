val scalaVersionString = "2.11.8"

name := "qb_pong"

version := "1.0"

scalaVersion := scalaVersionString

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.10" exclude("commons-logging", "commons-logging"),
//  "com.microsoft.sqlserver" % "sqljdbc4" % "4.0" exclude("commons-logging", "commons-logging"),
  "com.github.gilbertw1" %% "slack-scala-client" % "0.1.8" exclude("commons-logging", "commons-logging")
)

//lazy val sparkFatJar = project.in(file("subpojects/spark-fat-jar"))
//  .settings(Seq(scalaVersion := scalaVersionString))
//  .settings(assemblyMergeStrategy in assembly := {
//
//    // Fixes conflict between: asm3 AND asm4 ("last" picks asm4)
//    // technically ExclusionRule(organization = "asm") on 'hadoop-aws' should work... but it doesn't on CircleCI :(
//    case PathList("org", "objectweb", "asm", xs @ _*) => MergeStrategy.last
//
//    // Fixes conflict between: stax-api-1.0-2 AND geronimo-stax-api_1.0_spec-1.0.1
//    case PathList("javax", "xml", "stream", xs @ _*) => MergeStrategy.first
//
//    case PathList("org", "apache", "commons", xs @ _*) => MergeStrategy.first
//
//    case PathList("com", "typesafe", "akka", xs @ _*) => MergeStrategy.first
//
//    case PathList("com", "github", "gilbertw1", xs @ _*) => MergeStrategy.first
//
//    case x =>
//      val oldStrategy = (assemblyMergeStrategy in assembly).value
//      oldStrategy(x)
//  })
//  .dependsOn(RootProject(file(".")))
